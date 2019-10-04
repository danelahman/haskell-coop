{-# LANGUAGE DataKinds #-}

--
-- Example tests for the ML-style state runner in MLState.
--
-- In this example, the runner might raise a kill signal
-- if one tries to dereference a non-existent memory location.
--
-- For instance, in `test5` and `test8` below we deliberately
-- leak references out of an `mlTopLevel` block and feed them
-- to the next such block, where the references of course do 
-- not exist any more in the heap (that's initialised empty).
--

module SignalMLStateTests where

import Control.Monad.SignalRunner
import Control.Monad.SignalRunner.SignalMLState

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
    -- expected result "Exception: signal reached top level (RefNotInHeapSignal -- ref. with address Z)"

test6 :: Int -> Int -> User '[MLState] Zero (Ref Int,Ref Int)
test6 x y =
  do r <- alloc x;
     r' <- alloc y;
     return (r',r)

test8 =
  let (r,r') = mlTopLevel (test6 4 2) in
  mlTopLevel (test4 r r')
    -- expected result "Exception: signal reached top level (RefNotInHeapSignal -- ref. with address S Z)"