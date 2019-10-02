{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Runner.MLFPStateFwd (
  Nat, MemShape(..), Footprint(..), Addr(..),
  State, get, put, fpRunner, withFootprint
  ) where

import Control.Monad.Runner
import Control.Monad.Runner.MLState

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
-- Generic effects.
--
get :: (Typeable a) => Addr a memshape -> User '[State memshape] a
get addr = performU (Get addr)

put :: (Typeable a) => Addr a memshape -> a -> User '[State memshape] ()
put addr x = performU (Put addr x)

--
-- State runner for memshape-shaped footprint that just
-- forwards get-put operations to the external MLState runner.
--
fpCoOps :: State memshape r -> Kernel '[MLState] (Footprint memshape) r
fpCoOps (Get addr) =
  do mem <- getEnv;
     r <- return (lkpRef mem addr);
     execK ((!) r) return
fpCoOps (Put addr x) =
  do mem <- getEnv;
     r <- return (lkpRef mem addr);
     execK (r =:= x) return

fpRunner :: Runner '[State memshape] '[MLState] (Footprint memshape)
fpRunner = mkRunner fpCoOps

--
-- With-footprint construct for running a program only
-- on the given footprint of the whole ML-style memory.
--
withFootprint :: Footprint memshape -> User '[State memshape] a -> User '[MLState] a
withFootprint fp m =
  run
    fpRunner
    (return fp)
    m
    (\ x _ -> return x)