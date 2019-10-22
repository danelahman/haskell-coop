{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Control.Runner.FPState
Description : Runner for footprint-indexed global state, using effect typing to ensure that only the footprint is accessed
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module implements a runner that provides access to 
footprint-indexed global state (with n-many locations 
storing values of possibly different types). 

We use effect typing to ensure that only locations in a 
given footprint are allowed to be accessed. Specifically, 
the `State` effect that this module provides is indexed by 
a footprint, which comprises the shape of the memory, 
specifying how many locations/addresses are present, and 
what are the types of values that they store.
-}
module Control.Runner.FPState (
  Nat(..), MemShape(..), Memory(..), Addr(..),
  State(..), get, put, fpRunner, fpTopLevel
  ) where

import Control.Runner
import System.IO

-- | Type of natural numbers that we use for the size of the memory.
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

-- | Contents of the memory.
--
-- This is a memory shape indexed vector of values
-- stored in the memory locations determined by the
-- memory shape. The typing ensures that a location
-- meant to store values of type @a@ indeed stores 
-- values of type @a@.
data Memory :: forall memsize . MemShape memsize -> * where
  ME :: Memory ShE
  MC :: a -> Memory sh -> Memory (ShC a sh)

-- | Addresses of memory locations.
--
-- This is a type of finite sets (of locations)
-- indexed by a specific memory shape. The typing
-- ensures that the type of the address matches the
-- type of values stored in a given location.
data Addr a :: forall memsize . MemShape memsize -> * where
  AZ :: Addr a (ShC a sh)
  AS :: Addr a sh -> Addr a (ShC b sh)

-- | Looking up the value of an address in the memory.
lkp :: Memory memshape -> Addr a memshape -> a
lkp (MC x _) AZ = x
lkp (MC _ mem) (AS addr) = lkp mem addr

-- | Updating the value of an address in the memory.
upd :: Memory memshape -> Addr a memshape -> a -> Memory memshape
upd (MC _ mem) AZ x = MC x mem
upd (MC y mem) (AS addr) x = MC y (upd mem addr x)

--
-- Signature of the state effect for 0..m-1 references.
--

-- | A memory shape indexed effect for reading the value stored
-- at a given address, and writing a value to a given address.
data State (memshape :: MemShape memsize) :: * -> * where
  -- | Algebraic operation for reading the value stored at a given address.
  Get :: Addr a memshape -> State memshape a
  -- | Algebraic operation for writing the value to a given address.
  Put :: Addr a memshape -> a -> State memshape ()

-- | Generic effect for reading the value stored at a given address.
get :: Addr a memshape -> User '[State memshape] a
get addr = performU (Get addr)

-- | Generic effect for writing the value to a given address.
put :: Addr a memshape -> a -> User '[State memshape] ()
put addr x = performU (Put addr x)

-- | The co-operations of the runner `fpRunner`.
fpCoOps :: State memshape a -> Kernel sig (Memory memshape) a
fpCoOps (Get addr) =
  do mem <- getEnv;
     return (lkp mem addr)
fpCoOps (Put addr x) =
  do mem <- getEnv;
     setEnv (upd mem addr x)

-- | Runner that implements the `State` effect, by reading
-- values from its runtime state for the `Get` operations,
-- and updating the values in its runtime state for the `Put`
-- operations.
fpRunner :: Runner '[State memshape] sig (Memory memshape)
fpRunner = mkRunner fpCoOps

-- | Top-level for running user code in the `State` effect (as a pure value).
fpTopLevel :: Memory memshape -> User '[State memshape] a -> a
fpTopLevel init m =
  pureTopLevel (
    run
      fpRunner
      (return init)
      m
      (\ x _ -> return x)
  )