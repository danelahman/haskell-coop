{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Control.SignalRunner.ExcMLState
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

If one attempts to access a non-existent reference (for dereferencing 
or assignment), then the corresponding co-operation is going to send a
(kill) signal, which one can catch with the signal finaliser of `run`.
-}
module Control.SignalRunner.SignalMLState
  (
  Nat, Ref,
  Addr, addrOf, -- needed in `MonotonicMLState`
  MLState(..), S(..), Heap, 
  alloc, (!), deref, (=:=), assign,
  mlRunner, mlInitialiser, mlFinaliserVal, mlFinaliserExc, mlFinaliserSig, mlTopLevel,
  Typeable
  ) where

import Control.SignalRunner

import Data.Typeable

-- | Type of (kill) signals.
data S where
  -- | Signal sent when we observe that the given
  -- reference is not in the heap during dereferencing.
  RefNotInHeapInDerefSignal :: Ref a -> S
  -- | Signal sent when we observe that the given
  -- reference is not in the heap during assignment.
  RefNotInHeapInAssignSignal :: Ref a -> S

instance Show S where
  show (RefNotInHeapInDerefSignal r) = "RefNotInHeapInDerefSignal -- " ++ show r
  show (RefNotInHeapInAssignSignal r) = "RefNotInHeapInAssignSignal -- " ++ show r

-- | Type of natural numbers that we use for the address of memory references.
data Nat where
  Z :: Nat
  S :: Nat -> Nat

instance Eq Nat where
  Z == Z = True
  (S n) == (S m) = n == m
  _ == _ = False

instance Show Nat where
  show Z = "Z"
  show (S n) = "S " ++ show n

-- | Addresses of memory references (exposed because we need it to implement @MonotonicMLState@).
type Addr = Nat

-- | Type of memory references, restricted to only store
-- values of types satisfying the `Typeable` type class.
data Ref a where
  R :: (Typeable a) => Addr -> Ref a

instance Eq (Ref a) where
  R addr == R addr' = addr == addr'

instance Show (Ref a) where
  show r = "ref. with address " ++ show (addrOf r)

-- | Address of a reference (exposed because we need it to implement @MonotonicMLState@).
addrOf :: Ref a -> Addr
addrOf (R r) = r

-- | Memory is a partial map from references to `Typeable` values.
type Memory = forall a . (Typeable a) => Ref a -> Maybe a

-- | Type of heaps. These comprise a partial map from
-- memory references to values, and the address of
-- the next fresh memory reference to be allocated.
data Heap = H { memory :: Memory, nextAddr :: Addr }

-- | Reading the value of a memory reference in the heap.
--
-- It returns an optional value, depending on whether
-- the reference was present in the heap or not.
heapSel :: (Typeable a) => Heap -> Ref a -> Maybe a
heapSel h r = memory h r

-- | Updating the value of a memory reference in the memory.
memUpd :: (Typeable a) => Memory -> Ref a -> a -> Memory
memUpd mem r x r' =
  case cast x of
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
data MLState :: * -> * where
  -- | Algebraic operation for allocating a fresh memory reference.
  Alloc  :: (Typeable a) => a -> MLState (Ref a)
  -- | Algebraic operation for dereferencing a memory reference.
  Deref  :: (Typeable a) => Ref a -> MLState a
  -- | Algebraic operation for assiging a value to a memory reference.
  Assign :: (Typeable a) => Ref a -> a -> MLState ()

-- | Generic effect for allocating a fresh memory reference.
alloc :: (Typeable a,Member MLState sig) => a -> User sig e (Ref a)
alloc init = tryWithU (focus (performU (Alloc init))) return impossible

-- | Generic effect for dereferencing a memory reference.
(!) :: (Typeable a,Member MLState sig) => Ref a -> User sig e a
(!) r = tryWithU (focus (performU (Deref r))) return impossible

-- | Generic effect for dereferencing a memory reference (synonym of @(!)@).
deref r = (!) r -- used with qualified module names

-- | Generic effect for assigning a value to a memory reference.
(=:=) :: (Typeable a,Member MLState sig) => Ref a -> a -> User sig e ()
(=:=) r x = tryWithU (focus (performU (Assign r x))) return impossible

-- | Generic effect for assigning a value to a memory reference (synonym of @(=:=)@).
assign r x = r =:= x -- used with qualified module names

-- | The co-operations of the runner `mlRunner`.
mlCoOps :: MLState a -> Kernel sig Zero S Heap a
mlCoOps (Alloc init) =
  do h <- getEnv;
     (r,h') <- return (heapAlloc h init);
     setEnv h';
     return r
mlCoOps (Deref r) =
  do h <- getEnv;
     maybe
       (kill (RefNotInHeapInDerefSignal r))
       (\ x -> return x)
       (heapSel h r)
mlCoOps (Assign r x) =
  do h <- getEnv;
     maybe
       (kill (RefNotInHeapInAssignSignal r))
       (\ _ -> setEnv (heapUpd h r x))
       (heapSel h r)

-- | Runner that implements the `MLState` effect.
--
-- Its runtime state is a heap (see `Heap`), and its
-- co-operations call the corresponding allocation,
-- dereferencing, and assignment operations on the heap.
--
-- In the co-operation `Deref`, if the reference is
-- not present in the heap, the (kill) signal 
-- `RefNotInHeapInDerefSignal` gets sent.
--
-- In the co-operation `Assign`, if the reference is
-- not present in the heap, the (kill) signal 
-- `RefNotInHeapInAssignSignal` gets sent.
mlRunner :: Runner '[MLState] sig S Heap
mlRunner = mkRunner mlCoOps

-- | Initialiser for the runner `mlRunner` that
-- initialises the heap with the empty partial map,
-- and sets the next address to be allocated to zero.
mlInitialiser :: User sig Zero Heap
mlInitialiser = return (H { memory = \ _ -> Nothing , nextAddr = Z })

-- | Finaliser for return values for the runner `mlRunner`, 
-- which discards the final value of the heap, and simply
-- passes on the return value.
mlFinaliserVal :: a -> Heap -> User sig Zero a
mlFinaliserVal x _ = return x

-- | Finaliser for exceptions for the runner `mlRunner`, 
-- which is vacuously defined because there are
-- no exceptions (the exceptions index is `Zero`).
mlFinaliserExc :: Zero -> Heap -> User sig Zero a
mlFinaliserExc e _ = impossible e

-- | Finaliser for signals for the runner `mlRunner`, 
-- which raises a Haskell runtime error to signify
-- that an uncaught signal reached the top level.
mlFinaliserSig :: S -> User sig Zero a
mlFinaliserSig s = error ("signal reached top level (" ++ show s ++ ")")

-- | Top level for running user code that can use ML-style state.
mlTopLevel :: User '[MLState] Zero a -> a
mlTopLevel m =
  pureTopLevel (
    run
      mlRunner
      mlInitialiser
      m
      mlFinaliserVal
      mlFinaliserExc
      mlFinaliserSig
  )