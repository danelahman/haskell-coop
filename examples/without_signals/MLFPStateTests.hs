{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : MLFPStateTests
Description : Example use cases of the runner for footprint-based use of ML-style state from `Control.Runner.MLFPState`
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides example use cases of the runner for footprint-based 
use of ML-style state from `Control.Runner.MLFPState`.
-}
module MLFPStateTests where

import Control.Runner
import Control.Runner.FPState
import Control.Runner.MLState
import Control.Runner.MLFPState

test1 :: User '[MLState] (String,String,String,Bool)
test1 =
  do r <- alloc "foobar";
     r' <- alloc "foo";
     r'' <- alloc "bar";
     r''' <- alloc True;
     withFootprint (FC r' (FC r'' FE)) (
         do s <- get AZ;
            s' <- get (AS AZ);
            put AZ s';
            put (AS AZ) s
       );
     s <- (!) r;
     s' <- (!) r';
     s'' <- (!) r'';
     x <- (!) r''';
     return (s,s',s'',x)

test2 = mlTopLevel test1 -- expected result ("foobar","bar","foo",True)