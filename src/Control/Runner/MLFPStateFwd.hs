{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Control.Runner.MLFPState
Description : Using a runner to run a footprint of general, external ML-style state 
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module uses the `fpRunner` to run a footprint of general, 
external ML-style state (see `MLState` for details).
-}
module Control.Runner.MLFPStateFwd (
  Nat(..), MemShape(..), Footprint(..), Addr(..),
  State(..), get, put, fpRunner, withFootprint
  ) where

import Control.Runner
import Control.Runner.MLState

import Data.Typeable
import System.IO

-- | Type of natural numbers that we use for the footprint size.
data Nat where
  Z :: Nat
  S :: Nat -> Nat

-- | Shape of the memory.
--
-- This is a length-indexed vector of the types that 
-- the memory locations store in the memory.
data MemShape :: Nat -> * where
  ShE :: MemShape Z
  ShC :: * -> MemShape n -> MemShape (S n)

-- | Footprint is a memory shape indexed vector of ML-style
-- references, with the typing ensuring that types of
-- the references match locations in the memory shape.
data Footprint :: forall memsize . MemShape memsize -> * where
  FE :: Footprint ShE
  FC :: (Typeable a) => Ref a -> Footprint sh -> Footprint (ShC a sh)

-- | Addresses of memory locations.
--
-- This is a type of finite sets (of locations)
-- indexed by a specific memory shape. The typing
-- ensures that the type of the address matches the
-- type of values stored in a given location.
data Addr a :: forall memsize . MemShape memsize -> * where
  AZ :: (Typeable a) => Addr a (ShC a sh)
  AS :: (Typeable a,Typeable b) => Addr a sh -> Addr a (ShC b sh)

-- | Looking up the reference in a given footprint by its address.
lkpRef :: (Typeable a) => Footprint memshape -> Addr a memshape -> Ref a
lkpRef (FC r _) AZ = r
lkpRef (FC _ mem) (AS addr) = lkpRef mem addr

-- | A memory shape indexed effect for reading the value stored
-- at a given address, and writing a value to a given address.
data State (memshape :: MemShape memsize) :: * -> * where
  -- | Algebraic operation for reading the value stored at a given address.
  Get :: (Typeable a) => Addr a memshape -> State memshape a
  -- | Algebraic operation for writing the value to a given address.
  Put :: (Typeable a) => Addr a memshape -> a -> State memshape ()

-- | Generic effect for reading the value stored at a given address.
get :: (Typeable a) => Addr a memshape -> User '[State memshape] a
get addr = performU (Get addr)

-- | Generic effect for writing the value to a given address.
put :: (Typeable a) => Addr a memshape -> a -> User '[State memshape] ()
put addr x = performU (Put addr x)

-- | The co-operations of the runner `fpRunner`.
fpCoOps :: State memshape r -> Kernel '[MLState] (Footprint memshape) r
fpCoOps (Get addr) =
  do mem <- getEnv;
     r <- return (lkpRef mem addr);
     user ((!) r) return
fpCoOps (Put addr x) =
  do mem <- getEnv;
     r <- return (lkpRef mem addr);
     user (r =:= x) return

-- | Runner that implements the global state effect `State`
-- in an external context that provides the `MLState` effect.
--
-- As its runtime state, this runner stores a footprint of
-- ML-style references. The co-operations for `Get` and
-- `Put` delegate the operations to some enveloping runner
-- that implements the `MLState` effect of ML-style state.
fpRunner :: Runner '[State memshape] '[MLState] (Footprint memshape)
fpRunner = mkRunner fpCoOps

-- | Scoped running of user code on a given footprint of
-- general, external ML-style memory, using `fpRunner`.
withFootprint :: Footprint memshape -> User '[State memshape] a -> User '[MLState] a
withFootprint fp m =
  run
    fpRunner
    (return fp)
    m
    (\ x _ -> return x)