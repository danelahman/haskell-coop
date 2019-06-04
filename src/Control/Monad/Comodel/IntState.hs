{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Comodel.IntState (
  Nat(..), Addr(..),

  State, get, put, 
  Memory, withNewRef, runSt
  ) where

import Control.Monad.Comodel
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
-- Signature of the state effect for 0..m-1 references.
--
-- For simplicity, currently for storing only integers.
--
data State (memsize :: Nat) :: * -> * where
  Get :: Addr memsize -> State memsize Int
  Put :: Addr memsize -> Int -> State memsize ()

--
-- Human-readable syntax for State n operations.
--
get :: Addr memsize -> Comp '[State memsize] Int
get addr = perform (Get addr)

put :: Addr memsize -> Int -> Comp '[State memsize] ()
put addr x = perform (Put addr x)

--
-- ST comodel for memsize-many references
--
type Memory = Int

stCoOps :: State (S memsize) r -> Memory -> Comp '[State memsize] (r,Memory)

stCoOps (Get AZ) mem = return (mem,mem)
stCoOps (Get (AS addr)) mem =
  do x <- get addr; return (x,mem)
stCoOps (Put AZ x) mem = return ((),x)
stCoOps (Put (AS addr) x) mem =
  do _ <- put addr x; return ((),mem)

stComodel :: Comodel '[State memsize] '[State (S memsize)] Memory
stComodel = mkComodel stCoOps

--
-- ST memsize <-> ST (S memsize) lens.
--
stInitially :: Int -> Comp '[State memsize] Memory
stInitially init = return init

stFinally :: Memory -> a -> Comp '[State memsize] a
stFinally _ x = return x

stLens :: Int -> IFLens '[State memsize] Memory a a
stLens init = mkIFLens (stInitially init) stFinally

--
-- Derived with-new-ref construct, which extends the memory
-- with a new reference initialized to the given value init.
--
withNewRef :: Int -> Comp '[State (S memsize)] a -> Comp '[State memsize] a
withNewRef init c =
  run
    stComodel
    (stLens init)
    c

--
-- Top-level comodel in the pure context.
--
-- A bit of a temporary solution. Much better would like
-- to have Comp '[] a isomorphic to Comp '[State Z] a.
--
topCoOps :: State Z r -> Memory -> Comp '[] (r,Memory)

topCoOps (Get _) mem = return (mem,mem)
topCoOps (Put _ v) mem = return (() , v)

topComodel :: Comodel '[] '[State Z] Memory
topComodel = mkComodel topCoOps

--
-- Top-level lens from pure computations to State Z computations.
--
-- A bit of a temporary solution. Much better would like
-- to have Comp '[] a isomorphic to Comp '[State Z] a.
--
topInitially :: Comp '[] (Memory)
topInitially = return 0

topFinally :: Memory -> a -> Comp '[] a
topFinally _ x = return x

topLens :: IFLens '[] Memory a a
topLens = mkIFLens topInitially topFinally

--
-- Top-level running of empty-state computations.
--
runSt :: Comp '[State Z] a -> a
runSt c =
  runPure (
    run
      topComodel
      topLens
      c
  )