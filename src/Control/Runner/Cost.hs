{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Control.Runner.Cost
Description : Runner for instrumenting a user computation with a simple cost model
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module implements a runner that provides a means
to instrument user code with a very simple cost model 
that simply counts the total number of operation calls 
that the user code makes, with the corresponding 
finaliser `costFinaliser` then reporting the final cost.

For simplicity, the finaliser `costFinaliser`
simply returns a pair of the user code's return value 
and the final cost of the computation. One can of course
envisage both more elaborate cost models, but also 
finalisers that act on the final cost with other effects, 
e.g., by writing the final cost of the computation to IO.
-}
module Control.Runner.Cost (
  costRunner, costInitialiser, costFinaliser, costInstrumentation
  ) where

import Control.Runner

-- | The co-operations of the runner `costRunner`.
costCoOps :: Member eff sig => eff a -> Kernel sig Int a
costCoOps e =
  do c <- getEnv;
     setEnv (c + 1);
     performK e

-- | Runner that instruments a user computation with a simple
-- cost model that simply counts the number of operation calls
-- the user code makes (storing the count in its runtime state).
costRunner :: Member eff sig => Runner '[eff] sig Int
costRunner = mkRunner costCoOps

-- | Initialiser for the runner `costRunner`.
--
-- It sets the number of operation calls to zero.
costInitialiser :: User sig Int
costInitialiser = return 0

-- | Finaliser for the runner `costRunner`.
--
-- It returns the pair of the return value and the
-- final cost of the user computation that was run.
costFinaliser :: a -> Int -> User sig (a,Int)
costFinaliser x c = return (x,c)

-- | Sugar for inserting the runner `costRunner` inbetween
-- the user code @m@ and some enveloping runner.
--
-- As it stands, `costInstrumentation` is defined for a single
-- effect @eff@. For instrumenting code that uses more than one
-- effect, one can union the runner `costRunner` with itself
-- using `unionRunners` suitably many times.
costInstrumentation :: Member eff sig => User '[eff] a -> User sig (a,Int)
costInstrumentation m =
  run
    costRunner
    costInitialiser
    m
    costFinaliser