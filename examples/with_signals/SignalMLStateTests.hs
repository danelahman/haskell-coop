{-# LANGUAGE DataKinds #-}

{-|
Module      : SignalMLStateTests
Description : Example use cases of the runner for ML-style state from `Control.SignalRunner.SignalMLState`
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides example use cases of the runner 
for ML-style state from `Control.SignalRunner.SignalMLState`.

In this example, the runner might raise a kill signal
if one tries to dereference a non-existent memory location.

For instance, below we deliberately leak references out of 
an `mlTopLevel` block and feed them to the next such block, 
where the references of course do not exist any more in the 
heap (that's initialised empty), and cause a signal to be sent.
-}
module SignalMLStateTests where

import Control.SignalRunner
import Control.SignalRunner.SignalMLState

test1 :: Int -> Int -> User '[MLState] Zero (Int,Int)
test1 x y =
  do r <- alloc x;
     r' <- alloc y;
     x' <- (!) r;
     y' <- (!) r';
     return (x',y')

test2 = mlTopLevel (test1 4 2) -- expected result (4,2)

test3 :: Int -> Int -> User '[MLState] Zero (Ref Int,Ref Int)
test3 x y =
  do r <- alloc x;
     r' <- alloc y;
     return (r,r')

test4 :: Ref Int -> Ref Int -> User '[MLState] Zero (Int,Int)
test4 r r' =
  do x' <- (!) r;
     y' <- (!) r';
     return (x',y')

test5 =
  let (r,r') = mlTopLevel (test3 4 2) in
  mlTopLevel (test4 r r')
    -- expected result "Exception: signal reached top level (RefNotInHeapInDerefSignal -- ref. with address Z)"

test6 :: Int -> Int -> User '[MLState] Zero (Ref Int,Ref Int)
test6 x y =
  do r <- alloc x;
     r' <- alloc y;
     return (r',r)

test7 =
  let (r,r') = mlTopLevel (test6 4 2) in
  mlTopLevel (test4 r r')
    -- expected result "Exception: signal reached top level (RefNotInHeapInDerefSignal -- ref. with address S Z)"

test8 :: Ref Int -> Ref Int -> Int -> Int -> User '[MLState] Zero (Int,Int)
test8 r r' x y =
  do r =:= x ;
     r' =:= y;
     return (x,y)

test9 =
  let (r,r') = mlTopLevel (test3 4 2) in
  mlTopLevel (test8 r r' 2 4)
    -- expected result "Exception: signal reached top level (RefNotInHeapInAssignSignal -- ref. with address Z)"

test10 =
  let (r,r') = mlTopLevel (test6 4 2) in
  mlTopLevel (test8 r r' 2 4)
    -- expected result "Exception: signal reached top level (RefNotInHeapInAssignSignal -- ref. with address S Z)"
