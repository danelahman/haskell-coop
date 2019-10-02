{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Comodel.MLState
  (
  Ref, MLState, mlComodel, mlLens,
  alloc, (!), (=:=), topLevel
  ) where

import Control.Monad.Comodel

import Data.Typeable
import System.IO

--
-- Datatypes of natural numbers (for memory addresses).
--
data Nat where
  Z :: Nat
  S :: Nat -> Nat

instance Eq Nat where
  Z == Z = True
  (S n) == (S m) = n == m
  _ == _ = False

--
-- Type of references.
--
data Ref a where
  R :: (Typeable a) => Nat -> a -> Ref a

mkRef :: (Typeable a) => Nat -> a -> Ref a
mkRef addr x = R addr x

addr_of :: Ref a -> Nat
addr_of (R r _) = r

initial :: Ref a -> a
initial (R _ x) = x

--
-- Type of heaps, with associated select, update, and alloc functions.
--
type Memory = forall a . (Typeable a) => Ref a -> Maybe a

data Heap = H { memory :: Memory, next_addr :: Nat }

heap_sel :: (Typeable a) => Heap -> Ref a -> a
heap_sel h r =
  case memory h r of
    Nothing -> initial r
    Just x -> x

mem_upd :: (Typeable a) => Memory -> Ref a -> a -> Memory
mem_upd mem r x r' =
  case cast x of
    Nothing -> mem r'
    Just y -> (
      if addr_of r == addr_of r'
      then Just y
      else mem r')

heap_upd :: (Typeable a) => Heap -> Ref a -> a -> Heap
heap_upd h r x = h { memory = mem_upd (memory h) r x }

heap_alloc :: (Typeable a) => Heap -> a -> (Ref a,Heap)
heap_alloc h init =
  let r = mkRef (next_addr h) init in 
  (r , H { memory = mem_upd (memory h) r init ,
           next_addr = S (next_addr h) })

--
-- Signature of ML-style state operations.
--
data MLState :: * -> * where
  Alloc  :: (Typeable a) => a -> MLState (Ref a)
  Deref  :: (Typeable a) => Ref a -> MLState a
  Assign :: (Typeable a) => Ref a -> a -> MLState ()

--
-- Human-readable syntactic sugar.
--
alloc :: (Typeable a,Member MLState iface) => a -> Comp iface (Ref a)
alloc init = focus (perform (Alloc init))

(!) :: (Typeable a,Member MLState iface) => Ref a -> Comp iface a
(!) r = focus (perform (Deref r))

(=:=) :: (Typeable a,Member MLState iface) => Ref a -> a -> Comp iface ()
(=:=) r x = focus (perform (Assign r x))

--
-- (Top-level) ML-style memory comodel.
--
mlCoOps :: MLState r -> Heap -> Comp iface (r,Heap)
mlCoOps (Alloc init) h = return (heap_alloc h init)
mlCoOps (Deref r) h    = return (heap_sel h r , h)
mlCoOps (Assign r x) h = return (() , heap_upd h r x)

mlComodel :: Comodel iface '[MLState] Heap
mlComodel = mkComodel mlCoOps

--
-- PURE <-> MLState lens.
--
mlInitially :: Comp iface Heap
mlInitially = return (H { memory = \ _ -> Nothing , next_addr = Z })

mlFinally :: Heap -> a -> Comp iface a
mlFinally _ x = return x

mlLens :: IFLens iface Heap a a
mlLens = mkIFLens mlInitially mlFinally

--
-- Top-Level running of the ML-style memory.
--
topLevel :: Comp '[MLState] a -> a
topLevel c =
  runPure (
    run
      mlComodel
      mlLens
      c
  )