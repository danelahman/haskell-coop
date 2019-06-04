{-# LANGUAGE DataKinds #-}

--
-- Example tests for the ML-style state comodel in MLState.
--

module MLStateTests where

import Control.Monad.Comodel
import Control.Monad.Comodel.MLState

import Data.Typeable

test1 :: Int -> Int -> Comp '[MLState] (Int,Int)
test1 x y =
  do r <- alloc x;
     r' <- alloc y;
     x' <- (!) r;
     y' <- (!) r';
     return (x',y')

test2 = topLevel (test1 4 2)

test3 :: Int -> Comp '[MLState] Int
test3 x =
  do r <- alloc x;
     r =:=  (x + 2);
     y <- (!) r;
     return y

test4 = topLevel (test3 4)

test5 :: (Typeable a) => Ref a -> Ref a -> Comp '[MLState] ()
test5 r r' =
  do x <- (!) r;
     y <- (!) r';
     r =:= y;
     r' =:= x

test6 :: Comp '[MLState] (String,String)
test6 = 
  do r <- alloc "foo";
     r' <- alloc "bar";
     test5 r r';
     s <- (!) r;
     s' <- (!) r';
     return (s,s')

test7 = topLevel test6

test8 :: String -> Int
test8 s = length s

test9 :: String -> (String -> Int) -> Comp '[MLState] Int
test9 s f =
  do r <- alloc f;
     _ <- test3 42;
     g <- (!) r;
     return (g s)

test10 = topLevel (test9 "foobar" length)