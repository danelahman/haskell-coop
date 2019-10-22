{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Control.SignalRunner.MonotonicMLState
Description : Runner for monotonic ML-style state (supporting allocation, dereferencing, and assignment)
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module implements a runner that provides monotonic ML-style state
that supports allocation of references, dereferencing references, and 
assignment to references. 

We implement monotonicity similarly to the [F*](https://www.fstar-lang.org) 
language, by assiging a preorder with each reference, and ensuring that 
any assignments to references follow their respective preorders. While F* 
uses dependent types to enforce such monotonicity statically, we use 
runners to make dynamic checks. 

The idea is that the runner provided by this module would be interposed 
between the runner `mlRunner` and the user code. It internally only 
stores a memory of preorders, and delegates any reference allocation, 
dereferencing, and assignment operations to the runner `mlRunner` 
(or any other runner implementing the `MLState` effect).

If one attempts to perform an assignment to a reference with a value 
that is not related to the old value of the reference by the respective 
preorder, then we send a corresponding signal (alternatively, one could 
instead raise an exception, so as to allow the user code to try the 
assignment again with a different value, e.g., if we do not care about 
leaking information about the exisiting value of the reference). 

Further, if we observe that the user tries assignment with a reference that has no 
preorder assigned, we also raise a signal and kill the user code being run.
-}
module Control.SignalRunner.MonotonicMLState
  (
  MonS(..), Preorder,
  Ref, MonMemory, 
  MonMLState(..), alloc, (!), (=:=),
  monRunner, monInitialiser, monFinaliserVal, monFinaliserExc, monFinaliserSig, monTopLevel,
  Typeable
  ) where

import Control.SignalRunner
import Control.SignalRunner.SignalMLState hiding (alloc, (!), (=:=))
import qualified Control.SignalRunner.SignalMLState as ML (alloc, deref, assign)

import Data.Typeable

-- | Type of (kill) signals.
data MonS where
  -- | Signal sent when we observe that a given
  -- reference has no preorder associated with it.
  MissingPreorderSignal :: Ref a -> MonS
  -- | Signal sent when we observe that an assignment
  -- to a reference would violate the respective preorder.
  MononicityViolationSignal :: Ref a -> MonS

instance Show MonS where
  show (MononicityViolationSignal r) = "MononicityViolationSignal -- " ++ show r
  show (MissingPreorderSignal r) = "MissingPreorderSignal -- " ++ show r

-- | Type of preorders (implicitly assumed to satisfy reflexivity and transitivity).
type Preorder a = a -> a -> Bool

-- | Type of memory which associates `Ref`-typed references with
-- preorder. It is a partial map from references to preorders.
newtype MonMemory = M { memory :: forall a . (Typeable a) => Ref a -> Maybe (Preorder a)}

-- | Looking up the preorder associated with a given reference.
memSel :: (Typeable a) => MonMemory -> Ref a -> Maybe (Preorder a)
memSel m r = memory m r

-- | Updating the preorder associated with a given reference.
memUpd :: (Typeable a) => MonMemory -> Ref a -> Preorder a -> MonMemory
memUpd m r p =
  M { memory =
        \ r' -> case cast p of
                Nothing -> memory m r'
                Just q -> (
                  if (addrOf r == addrOf r')
                  then Just q
                  else memory m r') }

-- | An effect for monotonic ML-style state.
data MonMLState :: * -> * where
  -- | Algebraic operation for allocating a fresh reference
  -- and associating a preorder with it.
  MonAlloc  :: (Typeable a) => a -> Preorder a -> MonMLState (Ref a)
  -- | Algebraic operation for dereferencing a reference.
  MonDeref  :: (Typeable a) => Ref a -> MonMLState a
  -- | Algebraic operation for assiging a value to a reference.
  MonAssign :: (Typeable a) => Ref a -> a -> MonMLState ()

-- | Generic effect for allocating a fresh reference.
alloc :: (Typeable a,Member MonMLState sig) => a -> Preorder a -> User sig e (Ref a)
alloc init rel = tryWithU (focus (performU (MonAlloc init rel))) return impossible

-- | Generic effect for dereferencing a reference.
(!) :: (Typeable a,Member MonMLState sig) => Ref a -> User sig e a
(!) r = tryWithU (focus (performU (MonDeref r))) return impossible

-- | Generic effect for assigning a value to a reference.
(=:=) :: (Typeable a,Member MonMLState sig) => Ref a -> a -> User sig e ()
(=:=) r x = tryWithU (focus (performU (MonAssign r x))) return impossible

-- | The co-operations of the runner `monRunner`.
monCoOps :: Member MLState sig => MonMLState a -> Kernel sig Zero MonS MonMemory a
monCoOps (MonAlloc init rel) =
  do r <- user (ML.alloc init) return impossible;
     m <- getEnv;
     m' <- return (memUpd m r rel);
     setEnv m';
     return r
monCoOps (MonDeref r) =
  user (ML.deref r) return impossible
monCoOps (MonAssign r y) =
  do x <- user (ML.deref r) return impossible;
     m <- getEnv;
     maybe
       (kill (MissingPreorderSignal r))
       (\ rel -> if (rel x y)
                 then (user (ML.assign r y) return impossible)
                 else (kill (MononicityViolationSignal r)))
       (memSel m r)

-- | Runner that implements the `MonMLState` effect.
--
-- Its runtime state is a memory of preorders (see `MonMemory`), 
-- and its co-operations call both the corresponding allocation,
-- dereferencing, and assignment operations on the memory,
-- and on some enveloping runner that implements the `MLState`
-- effect, e.g., such as `mlRunner`.
--
-- In the co-operation `MonAssign`, if there is no preorder
-- associated with the given reference, then the (kill) signal
-- `MissingPreorderSignal` gets sent, and user code being run is killed.
--
-- Further in the co-operation `MonAssign`, if the new value being
-- assigned to a reference is not related to its existing value,
-- then a (kill) signal `MononicityViolationSignal` gets sent.
monRunner :: Member MLState sig => Runner '[MonMLState] sig MonS MonMemory
monRunner = mkRunner monCoOps

-- | Initialiser for the runner `monRunner` that
-- initialises the memory with the empty partial map.
monInitialiser :: User sig Zero MonMemory
monInitialiser = return (M { memory = \ _ -> Nothing })

-- | Finaliser for return values for the runner `mlRunner`, 
-- which discards the final value of the memory, and simply
-- passes on the return value.
monFinaliserVal :: a -> MonMemory -> User sig Zero a
monFinaliserVal x _ = return x

-- | Finaliser for exceptions for the runner `mlRunner`, 
-- which is vacuously defined because there are
-- no exceptions (the exceptions index is `Zero`).
monFinaliserExc :: Zero -> MonMemory -> User sig Zero a
monFinaliserExc e _ = impossible e

-- | Finaliser for signals for the runner `mlRunner`, 
-- which raises a Haskell runtime error to signify
-- that an uncaught signal reached the top level.
monFinaliserSig :: MonS -> User sig Zero a
monFinaliserSig s = error ("signal reached (monotonic) top level (" ++ show s ++ ")")

-- | Top level for running user code that can use monotonic ML-style state.
monTopLevel :: User '[MonMLState] Zero a -> a
monTopLevel m =
  pureTopLevel (
    run
      mlRunner
      mlInitialiser
      (run
         monRunner
         monInitialiser
         m
         monFinaliserVal
         monFinaliserExc
         monFinaliserSig)
      mlFinaliserVal
      mlFinaliserExc
      mlFinaliserSig
  )
