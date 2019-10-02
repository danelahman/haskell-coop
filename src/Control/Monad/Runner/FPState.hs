{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Runner.FPState (
  Nat, MemShape(..), Memory(..), Addr(..),
  State, get, put, fpRunner, fpTopLevel
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
-- Generic effects.
--
get :: Addr a memshape -> User '[State memshape] a
get addr = performU (Get addr)

put :: Addr a memshape -> a -> User '[State memshape] ()
put addr x = performU (Put addr x)

--
-- State comodel for memshape-shaped memory and references.
--
fpCoOps :: State memshape a -> Kernel iface (Memory memshape) a
fpCoOps (Get addr) =
  do mem <- getEnv;
     return (lkp mem addr)
fpCoOps (Put addr x) =
  do mem <- getEnv;
     setEnv (upd mem addr x)

fpRunner :: Runner '[State memshape] iface (Memory memshape)
fpRunner = mkRunner fpCoOps

--
-- Top-level running of a footprint of memory. 
--
fpTopLevel :: Memory memshape -> User '[State memshape] a -> a
fpTopLevel init m =
  pureTopLevel (
    run
      fpRunner
      (return init)
      m
      (\ x _ -> return x)
  )