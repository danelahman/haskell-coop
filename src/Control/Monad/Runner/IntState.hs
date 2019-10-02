{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Runner.IntState (
  Nat(..), Addr(..),
  State, get, put, 
  Memory, withNewRef, runSt
  ) where

import Control.Monad.Runner
import System.IO

--
-- Datatypes of natural numbers (for memory size).
--
data Nat where
  Z :: Nat
  S :: Nat -> Nat

--
-- Datatype of addresses in a memory of given size.
--
data Addr :: Nat -> * where
  AZ :: Addr (S n)
  AS :: Addr n -> Addr (S n)

--
-- Signature of the state effect for 0..memsize-1 references.
--
-- For simplicity, storing only integers.
--
data State (memsize :: Nat) :: * -> * where
  Get :: Addr memsize -> State memsize Int
  Put :: Addr memsize -> Int -> State memsize ()

--
-- Generic effects for State n operations.
--
get :: Addr memsize -> User '[State memsize] Int
get addr = performU (Get addr)

put :: Addr memsize -> Int -> User '[State memsize] ()
put addr x = performU (Put addr x)

--
-- ST runner for memsize-many references
--
type Memory = Int

stCoOps :: State (S memsize) r -> Kernel '[State memsize] Memory r
stCoOps (Get AZ) = getEnv
stCoOps (Get (AS addr)) = performK (Get addr)
stCoOps (Put AZ x) = setEnv x
stCoOps (Put (AS addr) x) = performK (Put addr x)

stRunner :: Runner '[State (S memsize)] '[State memsize] Memory
stRunner = mkRunner stCoOps

--
-- Derived with-new-ref construct, which extends the memory
-- with a new reference initialized to the given value init.
--
withNewRef :: Int -> User '[State (S memsize)] a -> User '[State memsize] a
withNewRef init m =
  run stRunner (return init) m (\ x _ -> return x)

--
-- Top-level runner in the pure context.
--
-- A bit of a temporary solution. Much better would be to
-- have Kernel '[] a isomorphic to Kernel '[State Z] a.
--
topCoOps :: State Z r -> Kernel '[] () r
topCoOps (Get _) =
  error "Should not be possible to run `get` in empty state"
topCoOps (Put _ x) =
  error "Should not be possible to run `put` in empty state"

topRunner :: Runner '[State Z] '[] ()
topRunner = mkRunner topCoOps

--
-- Top-level running of empty-state computations.
--
runSt :: User '[State Z] a -> a
runSt m =
  pureTopLevel (
    run topRunner (return ()) m (\ x _ -> return x) 
  )
