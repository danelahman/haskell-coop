{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

--
-- Focussing on a given footprint of the entire ML-style state
-- using a runner. The focussing has a transactional flavour:
-- the initialisation copies the values of the given footprint
-- to the runner's state, and the finaliser copies the final
-- values for the footprint back to the ML-style state.
--

module Control.Monad.Runner.MLFPState (
  Footprint(..), withFootprint
  ) where

import Control.Monad.Runner
import Control.Monad.Runner.FPState
import Control.Monad.Runner.MLState

import System.IO

--
-- Footprint of memory is a list of references.
--
data Footprint :: forall memsize . MemShape memsize -> * where
  FE :: Footprint ShE
  FC :: (Typeable a) => Ref a -> Footprint sh -> Footprint (ShC a sh)

--
-- With-footprint construct for running a program only
-- on the footprint of the whole ML-style memory.
--
fpInitialiser :: Footprint memshape -> User '[MLState] (Memory memshape)
fpInitialiser FE = return ME
fpInitialiser (FC r fp) =
  do mem <- fpInitialiser fp;
     x <- (!) r;
     return (MC x mem)

fpFinaliser :: Footprint memshape -> a -> Memory memshape -> User '[MLState] a
fpFinaliser FE x _ = return x
fpFinaliser (FC r fp) x (MC y mem) =
  do z <- fpFinaliser fp x mem;
     r =:= y;
     return z

withFootprint :: Footprint memshape -> User '[State memshape] a -> User '[MLState] a
withFootprint fp m =
  run
    fpRunner
    (fpInitialiser fp)
    m
    (fpFinaliser fp)