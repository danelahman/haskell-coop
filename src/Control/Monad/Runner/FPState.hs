{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Comodel.FPState (
  Nat, MemShape(..), Memory(..), Addr(..),
  State, get, put, fpComodel, fpLens, runFp
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
-- Shape of the memory (types of values stored) of a given size.
--
data MemShape :: Nat -> * where
  ShE :: MemShape Z
  ShC :: * -> MemShape n -> MemShape (S n)

--
-- Contents of a memory corresponding to a memory shape.
--
data Memory :: forall memsize . MemShape memsize -> * where
  ME :: Memory ShE
  MC :: a -> Memory sh -> Memory (ShC a sh)

--
-- Datatype of addresses in a memory of given size.
--
data Addr a :: forall memsize . MemShape memsize -> * where
  AZ :: Addr a (ShC a sh)
  AS :: Addr a sh -> Addr a (ShC b sh)

--
-- Lookup and update operations on memories.
--
lkp :: Memory memshape -> Addr a memshape -> a
lkp (MC x _) AZ = x
lkp (MC _ mem) (AS addr) = lkp mem addr

upd :: Memory memshape -> Addr a memshape -> a -> Memory memshape
upd (MC _ mem) AZ x = MC x mem
upd (MC y mem) (AS addr) x = MC y (upd mem addr x)

--
-- Signature of the state effect for 0..m-1 references.
--
data State (memshape :: MemShape memsize) :: * -> * where
  Get :: Addr a memshape -> State memshape a
  Put :: Addr a memshape -> a -> State memshape ()

--
-- Human-readable syntax for State n operations.
--
get :: Addr a memshape -> Comp '[State memshape] a
get addr = perform (Get addr)

put :: Addr a memshape -> a -> Comp '[State memshape] ()
put addr x = perform (Put addr x)

--
-- State comodel for memshape-shaped memory and references.
--
fpCoOps :: State memshape r -> Memory memshape -> Comp iface (r,Memory memshape)
fpCoOps (Get addr) mem = return (lkp mem addr , mem)
fpCoOps (Put addr x) mem = return (() , upd mem addr x)

fpComodel :: Comodel iface '[State memshape] (Memory memshape)
fpComodel = mkComodel fpCoOps

--
-- PURE <-> State footprint lens.
--
fpInitially :: Memory memshape -> Comp '[] (Memory memshape)
fpInitially init = return init

fpFinally :: Memory memshape -> a -> Comp '[] a
fpFinally _ x = return x

fpLens :: Memory memshape -> IFLens '[] (Memory memshape) a a
fpLens init = mkIFLens (fpInitially init) fpFinally

--
-- Top-level running of a footprint of memory. 
--
runFp :: Memory memshape -> Comp '[State memshape] a -> a
runFp init c =
  runPure (
    run
      fpComodel
      (fpLens init)
      c
  )