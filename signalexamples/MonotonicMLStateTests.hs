{-# LANGUAGE DataKinds #-}

--
-- Tests for the monotonic state runner in MonotonicMLState.
--
-- In this example, the runner might raise a kill signal
-- (i)  if one tries to dereference a non-existent memory location;
--      this is inherited from the ML-state runner in SignalMLState, or
-- (ii) if one tries to assign while violating monotonicity.
--

module SignalMLStateTests where

import Control.SignalRunner
import Control.SignalRunner.MonotonicMLState

test1 :: Int -> User '[MonMLState] Zero Int
test1 x =
  do r <- alloc x (>);
     x' <- (!) r;
     return x'

test2 = monTopLevel (test1 42) -- expected result 42

test3 :: Int -> User '[MonMLState] Zero Int
test3 x =
  do r <- alloc x (<=);
     x' <- (!) r;
     r =:= (x' + 1);
     r =:= (x' + 2);
     x'' <- (!) r;
     return x''

test4 = monTopLevel (test3 42) -- expected result 44

test5 :: Int -> User '[MonMLState] Zero Int
test5 x =
  do r <- alloc x (<=);
     x' <- (!) r;
     r =:= (x' - 1);
     x'' <- (!) r;
     return x''

test6 = monTopLevel (test5 42)
  -- expected result "Exception: signal reached (monotonic) top level (MononicityViolationSignal -- ref. with address Z)"


test7 :: Int -> Int -> User '[MonMLState] Zero (Ref Int,Ref Int)
test7 x y =
  do r <- alloc x (<=);
     r' <- alloc y (<=);
     return (r,r')

test8 :: Ref Int -> Ref Int -> User '[MonMLState] Zero (Int,Int)
test8 r r' =
  do x' <- (!) r;
     y' <- (!) r';
     return (x',y')

test9 =
  let (r,r') = monTopLevel (test7 4 2) in
  monTopLevel (test8 r r')
    -- expected result "Exception: signal reached top level (RefNotInHeapInDerefSignal -- ref. with address Z)"
    -- this signal happens in the ML state layer and thus kills of the monotonic layer