{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

We allow a large class of Haskell values to be stored in our references, 
as long as they are instances of the `Typeable` type class. We use this 
restriction to be able to compare the types of references for equality, 
so as to be able to define decidable equality for references (`refEq`), 
which we in turn use when updating the values stored in the heap.

If one attempts to access a non-existent reference (for dereferencing 
or assignment), then the corresponding co-operation is going to raise an
exception, which one can catch with `tryWithU` or the finaliser of `run`.
-}
module Control.SignalRunner.ExcMLState
  (
  Ref, refEq, MLState(..), E(..), Heap, 
  alloc, (!), (=:=),
  mlRunner, mlInitialiser, mlFinaliserVal,
  mlFinaliserExc, mlFinaliserSig, mlTopLevel,
  Typeable
  ) where

import Control.SignalRunner

import Data.Typeable

-- | Type of exceptions.
data E where
  -- | Exception raised when we observe that the given
  -- reference is not in the heap during dereferencing.
  RefNotInHeapInDerefException :: Ref a -> E
  -- | Exception raised when we observe that the given
  -- reference is not in the heap during assignment.
  RefNotInHeapInAssignException :: Ref a -> E

instance Show E where
  show (RefNotInHeapInDerefException r) = "RefNotInHeapInDerefException -- " ++ show r
  show (RefNotInHeapInAssignException r) = "RefNotInHeapInAssignException -- " ++ show r

-- | Type of natural numbers that we use for the address of references.
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

-- | Addresses of references.
type Addr = Nat

-- | Type of references, restricted to only store
-- values of types satisfying the `Typeable` type class.
data Ref a where
  R :: (Typeable a) => Addr -> Ref a

instance Show (Ref a) where
  show r = "ref. with address " ++ show (addrOf r)

-- | Exposing the address of a reference (private to this module).
addrOf :: Ref a -> Addr
addrOf (R r) = r

-- | Decidable equality on references (of possibly different types).
--
-- If the references are deemed to be equal, the equality test also
-- returns a proof that their types are (propositionally) equal.
refEq :: (Typeable a,Typeable b) => Ref a -> Ref b -> Maybe (a :~: b)
refEq (r :: Ref a) (r' :: Ref b) =
  if (addrOf r == addrOf r')
  then eqT @a @b
  else Nothing

-- | Memory is a partial map from references to `Typeable` values.
type Memory = forall a . (Typeable a) => Ref a -> Maybe a

-- | Type of heaps. These comprise a partial map from
-- references to values, and the address of
-- the next fresh reference to be allocated.
data Heap = H { memory :: Memory, nextAddr :: Addr }

-- | Reading the value of a reference in the heap.
--
-- It returns an optional value, depending on whether
-- the reference was present in the heap or not.
heapSel :: (Typeable a) => Heap -> Ref a -> Maybe a
heapSel h r = memory h r

-- | Updating the value of a reference in the memory.
memUpd :: (Typeable a) => Memory -> Ref a -> a -> Memory
memUpd mem r x r' =
  case refEq r r' of
    Nothing -> mem r'
    Just Refl -> Just x

-- | Updatring the value of a reference in the heap.
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
  -- | Algebraic operation for allocating a fresh reference.
  Alloc  :: (Typeable a) => a -> MLState (Ref a)
  -- | Algebraic operation for dereferencing a reference, raises
  -- an exception in `E` when the reference is not present in the heap.
  Deref  :: (Typeable a) => Ref a -> MLState (Either a E)
  -- | Algebraic operation for assiging a value to a reference, raises
  -- an exception in `E` when the reference is not present in the heap.
  Assign :: (Typeable a) => Ref a -> a -> MLState (Either () E)

-- | Generic effect for allocating a fresh reference.
alloc :: (Typeable a,Member MLState sig) => a -> User sig e (Ref a)
alloc init = performU (Alloc init)

-- | Generic effect for dereferencing a reference.
(!) :: (Typeable a,Member MLState sig) => Ref a -> User sig E a
(!) r = do xe <- performU (Deref r);
           either return (\ e -> raiseU e) xe

-- | Generic effect for assigning a value to a reference.
(=:=) :: (Typeable a,Member MLState sig) => Ref a -> a -> User sig E ()
(=:=) r x = do xe <- performU (Assign r x);
               either return (\ e -> raiseU e) xe

-- | The co-operations of the runner `mlRunner`.
mlCoOps :: MLState a -> Kernel sig Zero Zero Heap a
mlCoOps (Alloc init) =
  do h <- getEnv;
     (r,h') <- return (heapAlloc h init);
     setEnv h';
     return r
mlCoOps (Deref r)    =
  do h <- getEnv;
     maybe
       (return (Right (RefNotInHeapInDerefException r)))
       (\ x -> return (Left x))
       (heapSel h r)
mlCoOps (Assign r x) =
  do h <- getEnv;
     maybe
       (return (Right (RefNotInHeapInAssignException r)))
       (\ _ -> do setEnv (heapUpd h r x); return (Left ()))
       (heapSel h r)

-- | Runner that implements the `MLState` effect.
--
-- Its runtime state is a heap (see `Heap`), and its
-- co-operations call the corresponding allocation,
-- dereferencing, and assignment operations on the heap.
--
-- In the co-operation `Deref`, if the reference is
-- not present in the heap, the exception 
-- `RefNotInHeapInDerefException` gets raised.
--
-- In the co-operation `Assign`, if the reference is
-- not present in the heap, the exception 
-- `RefNotInHeapInAssignException` gets raised.
mlRunner :: Runner '[MLState] sig Zero Heap
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
-- which discards the final value of the heap, and 
-- simply raises a Haskell runtime error to signify
-- that an uncaught exception reached the top level.
mlFinaliserExc :: E -> Heap -> User sig Zero a
mlFinaliserExc e _ = error ("exception reached top level (" ++ show e ++ ")")

-- | Finaliser for signals for the runner `mlRunner`, 
-- which is vacuously defined because there are
-- no signals (the signals index is `Zero`).
mlFinaliserSig :: Zero -> User sig Zero a
mlFinaliserSig = impossible

-- | Top level for running user code that can use ML-style state.
mlTopLevel :: User '[MLState] E a -> a
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