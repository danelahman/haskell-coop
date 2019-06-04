{-# LANGUAGE DataKinds #-}

--
-- Example tests for the integer-state comodels in IntState.
--

module IntStateTests where

import Control.Monad.Comodel
import Control.Monad.Comodel.IntState

one = AZ
two = AS AZ

test1 :: Comp '[State Z] Int
test1 =
  withNewRef 42 (
    do i <- get one;
       return i
  )

test2 = runSt test1

test3 :: Comp '[State Z] Int
test3 =
  withNewRef 4 (
    do _ <- withNewRef 2 (
              do i <- get two;         -- reading the outer reference
                 j <- get one;         -- reading the inner reference
                 _ <- put two (i + j)  -- writing to the outer reference
                 return ());
       k <- get one;
       return (k + k)
  )

test4 = runSt test3