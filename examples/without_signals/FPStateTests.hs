{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for the footprint-based state runner in FPState.
--

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
