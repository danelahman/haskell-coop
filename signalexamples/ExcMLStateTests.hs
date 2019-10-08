{-# LANGUAGE DataKinds #-}

--
-- Example tests for the ML-style state runner in MLState.
--
-- In this example, the runner might raise an exception
-- if one tries to dereference a non-existent memory location.
--
-- For instance, in `test5` and `test8` below we deliberately
-- leak references out of an `mlTopLevel` block and feed them
-- to the next such block, where the references of course do 
-- not exist any more in the heap (that's initialised empty).
--

module ExcMLStateTests where

import Control.SignalRunner
import Control.SignalRunner.ExcMLState

test1 :: Int -> Int -> User '[MLState] E (Int,Int)
test1 x y =
  do r <- alloc x;
     r' <- alloc y;
     x' <- (!) r;
     y' <- (!) r';
     return (x',y')

test2 = mlTopLevel (test1 4 2) -- expected result (4,2)

test3 :: Int -> Int -> User '[MLState] E (Ref Int,Ref Int)
test3 x y =
  do r <- alloc x;
     r' <- alloc y;
     return (r,r')

test4 :: Ref Int -> Ref Int -> User '[MLState] E (Int,Int)
test4 r r' =
  do x' <- (!) r;
     y' <- (!) r';
     return (x',y')

test5 =
  let (r,r') = mlTopLevel (test3 4 2) in
  mlTopLevel (test4 r r')
    -- expected result "Exception: exception reached top level (RefNotInHeapInDerefException -- ref. with address Z)"

test6 :: Int -> Int -> User '[MLState] E (Ref Int,Ref Int)
test6 x y =
  do r <- alloc x;
     r' <- alloc y;
     return (r',r)

test7 =
  let (r,r') = mlTopLevel (test6 4 2) in
  mlTopLevel (test4 r r')
    -- expected result "Exception: exception reached top level (RefNotInHeapInDerefException -- ref. with address S Z)"

test8 :: Ref Int -> Ref Int -> User '[MLState] E (Int,Int)
test8 r r' =
  do x' <- tryWithU ((!) r)
             return
             (\ e -> error ("intercepted an exception (" ++ show e ++ ")"));
     y' <- (!) r';
     return (x',y')

test9 =
  let (r,r') = mlTopLevel (test3 4 2) in
  mlTopLevel (test8 r r')
    -- expected result "Exception: intercepted an exception (RefNotInHeapInDerefException -- ref. with address Z)"

test10 :: Ref Int -> Ref Int -> Int -> Int -> User '[MLState] E (Int,Int)
test10 r r' x y =
  do r =:= x ;
     r' =:= y;
     return (x,y)

test11 =
  let (r,r') = mlTopLevel (test3 4 2) in
  mlTopLevel (test10 r r' 2 4)
    -- expected result "Exception: exception reached top level (RefNotInHeapInAssignException -- ref. with address Z)"

test12 =
  let (r,r') = mlTopLevel (test6 4 2) in
  mlTopLevel (test10 r r' 2 4)
    -- expected result "Exception: exception reached top level (RefNotInHeapInAssignException -- ref. with address S Z)"
