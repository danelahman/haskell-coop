{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Control.IntState
Description : Runner for integer-valued global state, using effect typing to ensure that only a given footprint is accessed
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module implements a runner that provides access to 
integer-valued global state indexed by memory size (with 
n-many locations, all storing integers).

We use effect typing to ensure that only locations in a 
given footprint are allowed to be accessed. Specifically, 
the `State` effect that this module provides is indexed by 
a footprint, the size of the memory, specifying how many 
locations/addresses are present, and thus can be accessed.
-}
module Control.Runner.IntState (
  Nat(..), Addr(..),
  State(..), get, put, 
  Memory, stRunner, withNewRef, topRunner, runSt
  ) where

import Control.Runner
import System.IO

-- | Type of natural numbers that we use for the size of the memory.
data Nat where
  Z :: Nat
  S :: Nat -> Nat

-- | Type of memory addresses of a given size.
data Addr :: Nat -> * where
  AZ :: Addr (S n)
  AS :: Addr n -> Addr (S n)

-- | An memory size indexed effect for reading the value stored at
-- a given memory address, and writing a value to a given memory address.
data State (memsize :: Nat) :: * -> * where
  -- | Algebraic operation for reading the value stored at a given memory address.
  Get :: Addr memsize -> State memsize Int
  -- | Algebraic operation for writing the value to a given memory address.
  Put :: Addr memsize -> Int -> State memsize ()

-- | Generic effect for reading the value stored at a given memory address.
get :: Addr memsize -> User '[State memsize] Int
get addr = performU (Get addr)

-- | Generic effect for writing the value to a given memory address.
put :: Addr memsize -> Int -> User '[State memsize] ()
put addr x = performU (Put addr x)

-- | The type of the runtime state of the runner `stRunner`.
type Memory = Int

-- | The co-operations of the runner `stRunner`.
stCoOps :: State (S memsize) a -> Kernel '[State memsize] Memory a
stCoOps (Get AZ) = getEnv
stCoOps (Get (AS addr)) = performK (Get addr)
stCoOps (Put AZ x) = setEnv x
stCoOps (Put (AS addr) x) = performK (Put addr x)

-- | Runner that implements the effect @State (S memsize)@
-- in an external context that provides the @State memsize@ effect.
--
-- The intuition is that every time we call `run` with `stRunner`,
-- we allocate a new "fresh" memory address. Internally then 
-- this runner keeps only the value of the most recently
-- allocated memory address in its runtime state, and
-- delegates `Get` and `Put` operations to all other
-- addresses to some enveloping runner (implementing @State memsize@).
stRunner :: Runner '[State (S memsize)] '[State memsize] Memory
stRunner = mkRunner stCoOps

-- | A scoped allocation of a fresh memory address.
--
-- The first `Int`-valued argument is the initial value stored
-- in the fresh memory address.
withNewRef :: Int
           -> User '[State (S memsize)] a
           -> User '[State memsize] a
withNewRef init m =
  run stRunner (return init) m (\ x _ -> return x)

-- | The co-operations of the runner `topRunner`.
topCoOps :: State Z r -> Kernel '[] () r
topCoOps (Get _) =
  error "Should not be possible to run `get` in empty state"
topCoOps (Put _ x) =
  error "Should not be possible to run `put` in empty state"

-- | Top level runner for running user code with the
-- `State` effect in the empty external signature.
topRunner :: Runner '[State Z] '[] ()
topRunner = mkRunner topCoOps

-- | Top-level running of user code with the `State`
-- effect. It simply wraps `pureTopLevel` around running
-- user code with `topRunner`. The given user code starts
-- with no memory addresses allocated, and can then
-- allocate new addresses using `withNewRef`.
runSt :: User '[State Z] a -> a
runSt m =
  pureTopLevel (
    run
      topRunner
      (return ())
      m
      (\ x _ -> return x) 
  )
