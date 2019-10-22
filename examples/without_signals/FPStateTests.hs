{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : FPStateTests
Description : Example use cases of the footprint-based runner for state from `Control.Runner.FPState`
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides example use cases of the footprint-based 
runners for state from `Control.Runner.FPState`.
-}
module FPStateTests where

import Control.Runner
import Control.Runner.FPState

test1 :: User '[State (ShC Int (ShC String ShE))] Int
test1 =
  do x <- get AZ;
     return x

test2 = fpTopLevel (MC 42 (MC "foo" ME)) test1 -- expected result 42

test3 :: User '[State (ShC Int (ShC String ShE))] (Int,String)
test3 =
  do s <- get (AS AZ);
     x <- get AZ;
     put AZ (x + 7);
     put (AS AZ) (s ++ "bar");
     x' <- get AZ;
     s' <- get (AS AZ);
     return (x',s')

test4 = fpTopLevel (MC 42 (MC "foo" ME)) test3 -- expected result (49,"foobar")
