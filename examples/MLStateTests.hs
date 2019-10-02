{-# LANGUAGE DataKinds #-}

--
-- Example tests for the ML-style state runner in MLState.
--

module MLStateTests where

import Control.Monad.Runner
import Control.Monad.Runner.MLState

--import Data.Typeable

test1 :: Int -> Int -> User '[MLState] (Int,Int)
test1 x y =
  do r <- alloc x;
     r' <- alloc y;
     x' <- (!) r;
     y' <- (!) r';
     return (x',y')

test2 = mlTopLevel (test1 4 2) -- expected result (4,2)

test3 :: Int -> User '[MLState] Int
test3 x =
  do r <- alloc x;
     r =:=  (x + 2);
     y <- (!) r;
     return y

test4 = mlTopLevel (test3 4) -- expected result 6

test5 :: (Typeable a) => Ref a -> Ref a -> User '[MLState] ()
test5 r r' =
  do x <- (!) r;
     y <- (!) r';
     r =:= y;
     r' =:= x

test6 :: User '[MLState] (String,String)
test6 = 
  do r <- alloc "foo";
     r' <- alloc "bar";
     test5 r r';
     s <- (!) r;
     s' <- (!) r';
     return (s,s') -- expected result ("bar","foo")

test7 = mlTopLevel test6

test8 :: String -> Int
test8 s = length s

test9 :: String -> (String -> Int) -> User '[MLState] Int
test9 s f =
  do r <- alloc f;  -- storing a higher-order (pure) function argument in the state
     x <- test3 42;
     g <- (!) r;
     return (g s + x) -- length s + 44

test10 = mlTopLevel (test9 "foobar" length) -- expected result 50