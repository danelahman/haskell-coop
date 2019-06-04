{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Comodel.MLFPStateFwd (
  Nat, MemShape(..), Footprint(..), Addr(..),
  State, get, put, fpComodel, fpLens, withFootprint
  ) where

import Control.Monad.Comodel
import Control.Monad.Comodel.MLState

import Data.Typeable
import System.IO

--
-- Datatypes of natural numbers (for footprint size).
--
data Nat where
  Z :: Nat
  S :: Nat -> Nat

--
-- Shape of the memory (types of values stored) of a given size.
--
data MemShape :: Nat -> * where
  ShE :: MemShape Z
  ShC :: * -> MemShape n -> MemShape (S n)

--
-- Footprint of memory is a list of references.
--
data Footprint :: forall memsize . MemShape memsize -> * where
  FE :: Footprint ShE
  FC :: (Typeable a) => Ref a -> Footprint sh -> Footprint (ShC a sh)

--
-- Datatype of addresses in a footprint of given size.
--
data Addr a :: forall memsize . MemShape memsize -> * where
  AZ :: (Typeable a) => Addr a (ShC a sh)
  AS :: (Typeable a,Typeable b) => Addr a sh -> Addr a (ShC b sh)

--
-- Lookup the reference corresponding to a given address.
--
lkpRef :: (Typeable a) => Footprint memshape -> Addr a memshape -> Ref a
lkpRef (FC r _) AZ = r
lkpRef (FC _ mem) (AS addr) = lkpRef mem addr

--
-- Signature of the state effect for 0..m-1 references.
--
data State (memshape :: MemShape memsize) :: * -> * where
  Get :: (Typeable a) => Addr a memshape -> State memshape a
  Put :: (Typeable a) => Addr a memshape -> a -> State memshape ()

--
-- Human-readable syntax for State n operations.
--
get :: (Typeable a) => Addr a memshape -> Comp '[State memshape] a
get addr = perform (Get addr)

put :: (Typeable a) => Addr a memshape -> a -> Comp '[State memshape] ()
put addr x = perform (Put addr x)

--
-- State comodel for memshape-shaped footprint that just
-- forwards get-put operations to the external MLState world.
--
fpCoOps :: State memshape r -> Footprint memshape -> Comp '[MLState] (r,Footprint memshape)
fpCoOps (Get addr) mem = let r = lkpRef mem addr in
                         do x <- (!) r;
                            return (x,mem)

fpCoOps (Put addr x) mem = let r = lkpRef mem addr in
                           do _ <- r =:= x;
                              return ((),mem)

fpComodel :: Comodel '[MLState] '[State memshape] (Footprint memshape)
fpComodel = mkComodel fpCoOps

--
-- Lens for storing the given footprint of the memory.
--
fpInitially :: Footprint memshape -> Comp '[MLState] (Footprint memshape)
fpInitially FE = return FE
fpInitially (FC r fp) =
  do mem <- fpInitially fp;
     return (FC r mem)

fpFinally :: Footprint memshape -> a -> Comp '[MLState] a
fpFinally _ x = return x

fpLens :: Footprint memshape -> IFLens '[MLState] (Footprint memshape) a a
fpLens fp = mkIFLens (fpInitially fp) fpFinally

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