{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Runner.MLState
  (
  Ref, MLState, mlRunner, mlTopLevel,
  alloc, (!), (=:=),
  Typeable
  ) where

import Control.Monad.Runner

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
-- Typed references.
--
-- We restrict ourselves to references storing
-- `Typeable` values so as to be able to use
-- explicit type casting in `mem_upd` to decide
-- the equality of two typed references.
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
alloc :: (Typeable a,Member MLState iface) => a -> User iface (Ref a)
alloc init = focus (performU (Alloc init))

(!) :: (Typeable a,Member MLState iface) => Ref a -> User iface a
(!) r = focus (performU (Deref r))

(=:=) :: (Typeable a,Member MLState iface) => Ref a -> a -> User iface ()
(=:=) r x = focus (performU (Assign r x))

--
-- ML-style memory runner.
--
mlCoOps :: MLState r -> Kernel iface Heap r
mlCoOps (Alloc init) =
  do h <- getEnv;
     (r,h') <- return (heap_alloc h init);
     setEnv h';
     return r
mlCoOps (Deref r)    =
  do h <- getEnv;
     return (heap_sel h r)
mlCoOps (Assign r x) =
  do h <- getEnv;
     setEnv (heap_upd h r x);
     return ()

mlRunner :: Runner '[MLState] iface Heap
mlRunner = mkRunner mlCoOps

--
-- Top-Level running of the ML-style memory.
--
mlTopLevel :: User '[MLState] a -> a
mlTopLevel m =
  pureTopLevel (
    run
      mlRunner
      (return (H { memory = \ _ -> Nothing , next_addr = Z }))
      m
      (\ x _ -> return x)
  )
