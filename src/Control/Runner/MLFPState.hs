{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Control.Runner.MLFPState
Description : Using a runner to locally run a footprint of general, external ML-style state 
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module uses the `fpRunner` to locally run a footprint of 
general, external ML-style state (see `MLState` for details).
-}
module Control.Runner.MLFPState (
  Footprint(..), withFootprint
  ) where

import Control.Runner
import Control.Runner.FPState
import Control.Runner.MLState

import System.IO

-- | Footprint is a memory shape indexed vector of ML-style
-- memory references, with the typing ensuring that types of
-- the memory references match locations in the memory shape.
data Footprint :: forall memsize . MemShape memsize -> * where
  FE :: Footprint ShE
  FC :: (Typeable a) => Ref a -> Footprint sh -> Footprint (ShC a sh)

-- | Initialiser for running the given footprint of ML-style
-- memory references locally using the runner `fpRunner.
--
-- The initialiser recurses through the given footprint, and
-- looks up the values stored in each of the memory references,
-- and assembles them into a memory which it then returns.
fpInitialiser :: Footprint memshape -> User '[MLState] (Memory memshape)
fpInitialiser FE = return ME
fpInitialiser (FC r fp) =
  do mem <- fpInitialiser fp;
     x <- (!) r;
     return (MC x mem)

-- | Finaliser for running the given footprint of ML-style
-- memory references locally using the runner `fpRunner.
--
-- The finaliser recurses through the given footprint,
-- and performs assignments to all the references in it, 
-- and then passes on the return value unchanged.
fpFinaliser :: Footprint memshape -> a -> Memory memshape -> User '[MLState] a
fpFinaliser FE x _ = return x
fpFinaliser (FC r fp) x (MC y mem) =
  do z <- fpFinaliser fp x mem;
     r =:= y;
     return z

-- | Scoped running of user code on a given footprint of
-- general, external ML-style memory, using `fpRunner`.
withFootprint :: Footprint memshape -> User '[State memshape] a -> User '[MLState] a
withFootprint fp m =
  run
    fpRunner
    (fpInitialiser fp)
    m
    (fpFinaliser fp)