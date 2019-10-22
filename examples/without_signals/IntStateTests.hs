{-# LANGUAGE DataKinds #-}

{-|
Module      : IntStateTests
Description : Example use cases of the runner for integer-valued state from `Control.Runner.IntState`
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides example use cases of the runner for 
integer-valued state from `Control.Runner.IntState`.
-}
module IntStateTests where

import Control.Runner
import Control.Runner.IntState

one = AZ
two = AS AZ

test1 :: User '[State Z] Int
test1 =
  withNewRef 42 (
    do i <- get one;
       return i
  )

test2 = runSt test1 -- expected result 42

test3 :: User '[State Z] Int
test3 =
  withNewRef 4 (
    do withNewRef 2 (
         do i <- get two;         -- reading the outer reference
            j <- get one;         -- reading the inner reference
            put two (i + j)  -- writing to the outer reference
            return ());
       k <- get one;
       return (k + k + 1)
  )

test4 = runSt test3 -- expected result 13