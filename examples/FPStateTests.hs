{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for the footprint-based state comodels in FPState.
--

module FPStateTests where

import Control.Monad.Comodel
import Control.Monad.Comodel.FPState

test1 :: Comp '[State (ShC Int (ShC String ShE))] Int
test1 =
  do x <- get AZ;
     return x

test2 = runFp (MC 42 (MC "foo" ME)) test1

test3 :: Comp '[State (ShC Int (ShC String ShE))] (Int,String)
test3 =
  do s <- get (AS AZ);
     x <- get AZ;
     put AZ (x + 7);
     put (AS AZ) (s ++ "bar");
     x' <- get AZ;
     s' <- get (AS AZ);
     return (x',s')

test4 = runFp (MC 42 (MC "foo" ME)) test3
