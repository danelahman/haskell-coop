{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Control.Runner.IntMLState
Description : Runner for integer-valued ML-style state (supporting allocation, dereferencing, and assignment)
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module implements a runner that provides integer-valued 
ML-style state, i.e., state that supports allocation of references, 
dereferencing a value of a reference, and assignment to a reference.
-}
module Control.Runner.IntMLState
  (
  Ref, Heap, IntMLState(..),
  alloc, (!), (=:=),
  intMLRunner, intMLInitialiser, intMLFinaliser, intMLTopLevel,
  ) where

import Control.Runner

-- | Type of natural numbers that we use for the address of references.
data Nat where
  Z :: Nat
  S :: Nat -> Nat

instance Eq Nat where
  Z == Z = True
  (S n) == (S m) = n == m
  _ == _ = False

-- | Addresses of references.
type Addr = Nat

-- | Type of references.
data Ref where
  R :: Addr -> Ref

-- | Exposing the address of a reference (private to this module).
addrOf :: Ref -> Addr
addrOf (R r) = r

-- | Memory if a partial map from references to integers.
type Memory = Ref -> Maybe Int

-- | Type of heaps. These comprise a partial map from
-- references to integers, and the address of
-- the next fresh reference to be allocated.
data Heap = H { memory :: Memory, nextAddr :: Addr }

-- | Reading the value of a reference in the heap.
heapSel :: Heap -> Ref -> Int
heapSel h r =
  case memory h r of
    Nothing -> error "reference not in the heap" -- raising a runtime error
    Just x -> x

-- | Updating the value of a reference in the memory.
memUpd :: Memory -> Ref -> Int -> Memory
memUpd m r x r' =
  if (addrOf r == addrOf r')
  then Just x
  else m r'

-- | Updatring the value of a reference
-- in the heap, with the given initial value.
heapUpd :: Heap -> Ref -> Int -> Heap
heapUpd h r x = h { memory = memUpd (memory h) r x }

-- | Allocating a fresh reference in the heap.
heapAlloc :: Heap -> Int -> (Ref,Heap)
heapAlloc h init =
  let r = R (nextAddr h) in 
  (r , H { memory = memUpd (memory h) r init ,
           nextAddr = S (nextAddr h) })

-- | An effect for integer-valued ML-style state.
data IntMLState a where
  -- | Algebraic operation for allocating a fresh reference.
  Alloc  :: Int -> IntMLState Ref
  -- | Algebraic operation for dereferencing a reference.
  Deref  :: Ref -> IntMLState Int
  -- | Algebraic operation for assiging a value to a reference.
  Assign :: Ref -> Int -> IntMLState ()

-- | Generic effect for allocating a fresh reference.
alloc :: Member IntMLState sig => Int -> User sig Ref
alloc init = focus (performU (Alloc init))

-- | Generic effect for dereferencing a reference.
(!) :: Member IntMLState sig => Ref -> User sig Int
(!) r = focus (performU (Deref r))

-- | Generic effect for assigning a value to a reference.
(=:=) :: Member IntMLState sig => Ref -> Int -> User sig ()
(=:=) r x = focus (performU (Assign r x))

-- | The co-operations of the runner `intMLRunner`.
intMLCoOps :: IntMLState a -> Kernel sig Heap a
intMLCoOps (Alloc init) =
  do h <- getEnv;
     (r,h') <- return (heapAlloc h init);
     setEnv h';
     return r
intMLCoOps (Deref r) =
  do h <- getEnv;
     return (heapSel h r)
intMLCoOps (Assign r x) =
  do h <- getEnv;
     setEnv (heapUpd h r x)

-- | Runner that implements the `IntMLState` effect.
--
-- Its runtime state is a heap (see `Heap`), and its
-- co-operations call the corresponding allocation,
-- dereferencing, and assignment operations on the heap.
intMLRunner :: Runner '[IntMLState] sig Heap
intMLRunner = mkRunner intMLCoOps

-- | Initialiser for the runner `intMLRunner` that
-- initialises the heap with the empty partial map,
-- and sets the next address to be allocated to zero.
intMLInitialiser :: User sig Heap
intMLInitialiser = return (H { memory = \ _ -> Nothing , nextAddr = Z })

-- | Finaliser for the runner `intMLRunner` that
-- discards the final value of the heap, and simply
-- passes on the return value.
intMLFinaliser :: a -> Heap -> User sig a
intMLFinaliser x _ = return x

-- | Top level for running user code that can use
-- integer-valued ML-style state.
intMLTopLevel :: User '[IntMLState] a -> a
intMLTopLevel m =
  pureTopLevel (
    run
      intMLRunner
      intMLInitialiser
      m
      intMLFinaliser
  )