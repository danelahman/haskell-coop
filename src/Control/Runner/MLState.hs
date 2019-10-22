{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Control.Runner.MLState
Description : Runner for general ML-style state (supporting allocation, dereferencing, and assignment)
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module implements a runner that provides general ML-style state
that supports allocation of references, dereferencing references, 
and assignment to references. 

We allow a large class of Haskell values to be stored in our memory 
references, as long as they are instances of the `Typable` type class. 
We use this restriction to be able to use the type-safe `cast` operation
as a means to compare the types of two references for equality. 
-}
module Control.Runner.MLState
  (
  Ref, MLState(..), Heap,
  alloc, (!), (=:=),
  mlRunner, mlInitialiser, mlFinaliser, mlTopLevel,
  Typeable
  ) where

import Control.Runner

import Data.Typeable

-- | Type of natural numbers that we use for the address of memory references.
data Nat where
  Z :: Nat
  S :: Nat -> Nat

instance Eq Nat where
  Z == Z = True
  (S n) == (S m) = n == m
  _ == _ = False

-- | Addresses of memory references.
type Addr = Nat

-- | Type of memory references, restricted to only store
-- values of types satisfying the `Typeable` type class.
data Ref a where
  R :: (Typeable a) => Addr -> Ref a

-- | Exposing the address of a reference (private to this module).
addrOf :: Ref a -> Addr
addrOf (R r) = r

-- | Memory is a partial map from references to `Typeable` values.
type Memory = forall a . (Typeable a) => Ref a -> Maybe a

-- | Type of heaps. These comprise a partial map from
-- memory references to values, and the address of
-- the next fresh memory reference to be allocated.
data Heap = H { memory :: Memory, nextAddr :: Addr }

-- | Reading the value of a memory reference in the heap.
heapSel :: (Typeable a) => Heap -> Ref a -> a
heapSel h r =
  case memory h r of
    Nothing -> error "reference not in the heap" -- raising a runtime error
    Just x -> x

-- | Updating the value of a memory reference in the memory.
memUpd :: (Typeable a) => Memory -> Ref a -> a -> Memory
memUpd mem r x r' =
  case cast x of -- using `cast` to (indirectly) compare the types of two references
    Nothing -> mem r'
    Just y -> (
      if (addrOf r == addrOf r')
      then Just y
      else mem r')

-- | Updatring the value of a memory reference in the heap.
heapUpd :: (Typeable a) => Heap -> Ref a -> a -> Heap
heapUpd h r x = h { memory = memUpd (memory h) r x }

-- | Allocating a fresh reference in the heap,
-- with the given initial value.
heapAlloc :: (Typeable a) => Heap -> a -> (Ref a,Heap)
heapAlloc h init =
  let r = R (nextAddr h) in 
  (r , H { memory = memUpd (memory h) r init ,
           nextAddr = S (nextAddr h) })

-- | An effect for general ML-style state.
data MLState a where
  -- | Algebraic operation for allocating a fresh memory reference.
  Alloc  :: (Typeable a) => a -> MLState (Ref a)
  -- | Algebraic operation for dereferencing a memory reference.
  Deref  :: (Typeable a) => Ref a -> MLState a
  -- | Algebraic operation for assiging a value to a memory reference.
  Assign :: (Typeable a) => Ref a -> a -> MLState ()

-- | Generic effect for allocating a fresh memory reference.
alloc :: (Typeable a,Member MLState sig) => a -> User sig (Ref a)
alloc init = focus (performU (Alloc init))

-- | Generic effect for dereferencing a memory reference.
(!) :: (Typeable a,Member MLState sig) => Ref a -> User sig a
(!) r = focus (performU (Deref r))

-- | Generic effect for assigning a value to a memory reference.
(=:=) :: (Typeable a,Member MLState sig) => Ref a -> a -> User sig ()
(=:=) r x = focus (performU (Assign r x))

-- | The co-operations of the runner `mlRunner`.
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

-- | Runner that implements the `MLState` effect.
--
-- Its runtime state is a heap (see `Heap`), and its
-- co-operations call the corresponding allocation,
-- dereferencing, and assignment operations on the heap.
mlRunner :: Runner '[MLState] sig Heap
mlRunner = mkRunner mlCoOps

-- | Initialiser for the runner `mlRunner` that
-- initialises the heap with the empty partial map,
-- and sets the next address to be allocated to zero.
mlInitialiser :: User sig Heap
mlInitialiser = return (H { memory = \ _ -> Nothing , nextAddr = Z })

-- | Finaliser for the runner `mlRunner` that
-- discards the final value of the heap, and simply
-- passes on the return value.
mlFinaliser :: a -> Heap -> User sig a
mlFinaliser x _ = return x

-- | Top level for running user code that can use ML-style state.
mlTopLevel :: User '[MLState] a -> a
mlTopLevel m =
  pureTopLevel (
    run
      mlRunner
      mlInitialiser
      m
      mlFinaliser
  )