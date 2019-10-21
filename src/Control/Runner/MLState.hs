{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

--
-- ML-style state implemented using runners, supporting allocation
-- lookup, and assignment of memory references. Using polymorphism
-- provided by Haskell, we store values with arbitrary types, though
-- we limit ourselves to storing Typable values, so as to be able
-- to use typecasting as a means for deciding type equality.
--

module Control.Runner.MLState
  (
  Ref, MLState(..), Heap,
  alloc, (!), (=:=),
  mlRunner, mlInitialiser, mlFinaliser, mlTopLevel,
  Typeable
  ) where

import Control.Runner

import Data.Typeable

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
-- explicit type casting in `memUpd` to decide
-- the equality of two typed references.
--
type Addr = Nat

data Ref a where
  R :: (Typeable a) => Addr -> Ref a

mkRef :: (Typeable a) => Addr -> Ref a
mkRef addr = R addr

addrOf :: Ref a -> Addr
addrOf (R r) = r

--
-- Type of heaps, with associated select, update, and alloc functions.
--
type Memory = forall a . (Typeable a) => Ref a -> Maybe a

data Heap = H { memory :: Memory, nextAddr :: Addr }

heapSel :: (Typeable a) => Heap -> Ref a -> a
heapSel h r =
  case memory h r of
    Nothing -> error "reference not in the heap"
    Just x -> x

memUpd :: (Typeable a) => Memory -> Ref a -> a -> Memory
memUpd mem r x r' =
  case cast x of
    Nothing -> mem r'
    Just y -> (
      if (addrOf r == addrOf r')
      then Just y
      else mem r')

heapUpd :: (Typeable a) => Heap -> Ref a -> a -> Heap
heapUpd h r x = h { memory = memUpd (memory h) r x }

heapAlloc :: (Typeable a) => Heap -> a -> (Ref a,Heap)
heapAlloc h init =
  let r = mkRef (nextAddr h) in 
  (r , H { memory = memUpd (memory h) r init ,
           nextAddr = S (nextAddr h) })

--
-- Signature of ML-style state operations.
--
data MLState :: * -> * where
  Alloc  :: (Typeable a) => a -> MLState (Ref a)
  Deref  :: (Typeable a) => Ref a -> MLState a
  Assign :: (Typeable a) => Ref a -> a -> MLState ()

--
-- Generic effects.
--
alloc :: (Typeable a,Member MLState sig) => a -> User sig (Ref a)
alloc init = focus (performU (Alloc init))

(!) :: (Typeable a,Member MLState sig) => Ref a -> User sig a
(!) r = focus (performU (Deref r))

(=:=) :: (Typeable a,Member MLState sig) => Ref a -> a -> User sig ()
(=:=) r x = focus (performU (Assign r x))

--
-- ML-style memory runner.
--
mlCoOps :: MLState a -> Kernel sig Heap a
mlCoOps (Alloc init) =
  do h <- getEnv;
     (r,h') <- return (heapAlloc h init);
     setEnv h';
     return r
mlCoOps (Deref r)    =
  do h <- getEnv;
     return (heapSel h r)
mlCoOps (Assign r x) =
  do h <- getEnv;
     setEnv (heapUpd h r x)

mlRunner :: Runner '[MLState] sig Heap
mlRunner = mkRunner mlCoOps

--
-- Top-Level running of the ML-style memory.
--
mlInitialiser :: User sig Heap
mlInitialiser = return (H { memory = \ _ -> Nothing , nextAddr = Z })

mlFinaliser :: a -> Heap -> User sig a
mlFinaliser x _ = return x

mlTopLevel :: User '[MLState] a -> a
mlTopLevel m =
  pureTopLevel (
    run
      mlRunner
      mlInitialiser
      m
      mlFinaliser
  )