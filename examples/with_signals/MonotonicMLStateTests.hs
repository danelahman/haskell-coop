{-# LANGUAGE DataKinds #-}

{-|
Module      : MonotonicMLStateTests
Description : Example use cases of the runner for ML-style state from `Control.SignalRunner.MonotonicMLState`
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides example use cases of the runner 
for ML-style state from `Control.SignalRunner.MonotonicMLState`.

In these examples, the runner might raise a kill signal
(i)  if one tries to dereference a non-existent memory location;
     this is inherited from the runner in `SignalMLState`, or
(ii) if one tries assign to a reference that has no preorder 
     associated with it.

Further, if one tries to assign a new value to a reference that 
is not related to the reference's existing value by the associated 
preorder, then we raise an exception, that the user code can catch 
(if it so wishes) to, e.g., try again with a different value.
-}
module MonotonicMLStateTests where

import Control.SignalRunner
import Control.SignalRunner.MonotonicMLState

test1 :: Int -> User '[MonMLState] MonE Int
test1 x =
  do r <- alloc x (>);
     x' <- (!) r;
     return x'

test2 = monTopLevel (test1 42) -- expected result 42

test3 :: Int -> User '[MonMLState] MonE Int
test3 x =
  do r <- alloc x (<=);
     x' <- (!) r;
     r =:= (x' + 1);
     r =:= (x' + 2);
     x'' <- (!) r;
     return x''

test4 = monTopLevel (test3 42) -- expected result 44

test5 :: Int -> User '[MonMLState] MonE Int
test5 x =
  do r <- alloc x (<=);
     x' <- (!) r;
     r =:= (x' - 1);
     x'' <- (!) r;
     return x''

test6 = monTopLevel (test5 42)
  -- expected result "Exception: exception reached (monotonic) top level (MononicityViolationException -- ref. with address Z)"

test7 :: Int -> Int -> User '[MonMLState] MonE (Ref Int,Ref Int)
test7 x y =
  do r <- alloc x (<=);
     r' <- alloc y (<=);
     return (r,r')

test8 :: Ref Int -> Ref Int -> User '[MonMLState] MonE (Int,Int)
test8 r r' =
  do x' <- (!) r;
     y' <- (!) r';
     return (x',y')

test9 =
  let (r,r') = monTopLevel (test7 4 2) in
  monTopLevel (test8 r r')
    -- expected result "Exception: signal reached top level (RefNotInHeapInDerefSignal -- ref. with address Z)"
    -- this signal happens in the ML-style state layer and thus kills off the monotonic state layer

test10 :: Int -> User '[MonMLState] MonE Int
test10 x =
  do r <- alloc x (<=);
     x' <- (!) r;
     tryWithU
       (r =:= (x' - 1))
       (\ x -> return x)
       (\ e -> r =:= (x' + 1));
     x'' <- (!) r;
     return x''

test11 = monTopLevel (test10 42)  -- expected result 43

test12 :: Int -> User '[MonMLState] MonE Int
test12 x =
  do r <- alloc x (<=);
     x' <- (!) r;
     tryWithU
       (r =:= (x' + 2))
       (\ x -> return x)
       (\ e -> r =:= (x' - 1));
     x'' <- (!) r;
     return x''

test13 = monTopLevel (test12 42)  -- expected result 44
