{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Comodel.MLFPState (Footprint(..), withFootprint) where

import Control.Monad.Comodel
import Control.Monad.Comodel.FPState hiding (fpInitially, fpFinally, fpLens)
import Control.Monad.Comodel.MLState

import Data.Typeable
import System.IO

--
-- Footprint of memory is a list of references.
--
data Footprint :: forall memsize . MemShape memsize -> * where
  FE :: Footprint ShE
  FC :: (Typeable a) => Ref a -> Footprint sh -> Footprint (ShC a sh)

--
-- Lens for fetching the initial values of the footprint
-- and for writing the final values of the footprint back.
--
fpInitially :: Footprint memshape -> Comp '[MLState] (Memory memshape)
fpInitially FE = return ME
fpInitially (FC r fp) =
  do mem <- fpInitially fp;
     x <- (!) r;
     return (MC x mem)

fpFinally :: Footprint memshape -> Memory memshape -> a -> Comp '[MLState] a
fpFinally FE _ x = return x
fpFinally (FC r fp) (MC x mem) y =
  do z <- fpFinally fp mem y;
     r =:= x;
     return z

fpLens :: Footprint memshape -> IFLens '[MLState] (Memory memshape) a a
fpLens fp = mkIFLens (fpInitially fp) (fpFinally fp)

--
-- With-footprint construct for running a program only
-- on the footprint of the whole ML-style memory.
--
withFootprint :: Footprint memshape -> Comp '[State memshape] a -> Comp '[MLState] a
withFootprint fp c =
  run
    fpComodel
    (fpLens fp)
    c