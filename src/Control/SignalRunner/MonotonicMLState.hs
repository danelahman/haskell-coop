{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

--
-- A runner that enforces monotonicity upon an ML-style state
-- from `SignalMLState`. It does so by "slotting between" the 
-- user code and the ML-style state runner, using the latter 
-- to manage allocation, dereferencing, and assignment of
-- references, while itself keeping track of monotonicity.
--
-- Monotonicity is realised similarly to the F* language,
-- by associating a preorder with each memory location,
-- and ensuring that any assignments to a reference adhere
-- to a given preorder. While in F* this is done statically,
-- here we use runtime verification to achieve monotonicity.
--
-- The runner raises a signal if an assignment to a reference
-- is about to violate the preorder associated with it.
--

module Control.SignalRunner.MonotonicMLState
  (
  Ref, MonMLState(..), MonS(..), 
  alloc, (!), (=:=),
  monRunner, monInitialiser, monFinaliserVal, monFinaliserExc, monFinaliserSig, monTopLevel,
  Typeable
  ) where

import Control.SignalRunner
import Control.SignalRunner.SignalMLState hiding (alloc, (!), (=:=))
import qualified Control.SignalRunner.SignalMLState as ML (alloc, deref, assign)

import Data.Typeable

--
-- Kill signal(s).
--
data MonS where
  MissingPreorderSignal :: Ref a -> MonS
  MononicityViolationSignal :: Ref a -> MonS

instance Show MonS where
  show (MononicityViolationSignal r) = "MononicityViolationSignal -- " ++ show r
  show (MissingPreorderSignal r) = "MissingPreorderSignal -- " ++ show r

--
-- Type of preorders (with implicit reflexicity-transitivity constraints).
--
type Preorder a = a -> a -> Bool

--
-- Type of memory storing monotonicity preorders for ML-style references.
--
newtype MonMemory = M { memory :: forall a . (Typeable a) => Ref a -> Maybe (Preorder a)}

memSel :: (Typeable a) => MonMemory -> Ref a -> Maybe (Preorder a)
memSel m r = memory m r

memUpd :: (Typeable a) => MonMemory -> Ref a -> Preorder a -> MonMemory
memUpd m r p =
  M { memory =
        \ r' -> case cast p of
                Nothing -> memory m r'
                Just q -> (
                  if (addrOf r == addrOf r')
                  then Just q
                  else memory m r') }

--
-- Generic effects (we are hiding the generic effects of `SignalMLState`).
--
alloc :: (Typeable a,Member MonMLState sig) => a -> Preorder a -> User sig e (Ref a)
alloc init rel = tryWithU (focus (performU (MonAlloc init rel))) return impossible

(!) :: (Typeable a,Member MonMLState sig) => Ref a -> User sig e a
(!) r = tryWithU (focus (performU (MonDeref r))) return impossible

(=:=) :: (Typeable a,Member MonMLState sig) => Ref a -> a -> User sig e ()
(=:=) r x = tryWithU (focus (performU (MonAssign r x))) return impossible

--
-- Signature of monotonic ML-style state operations.
--
data MonMLState :: * -> * where
  MonAlloc  :: (Typeable a) => a -> Preorder a -> MonMLState (Ref a)
  MonDeref  :: (Typeable a) => Ref a -> MonMLState a
  MonAssign :: (Typeable a) => Ref a -> a -> MonMLState ()

--
-- Runner that tracks monotonicity for ML-style state.
--
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

monRunner :: Member MLState sig => Runner '[MonMLState] sig MonS MonMemory
monRunner = mkRunner monCoOps

--
-- Top-Level running of monotonic ML-style memory.
--
monInitialiser :: User sig Zero MonMemory
monInitialiser = return (M { memory = \ _ -> Nothing })

monFinaliserVal :: a -> MonMemory -> User sig Zero a
monFinaliserVal x _ = return x

monFinaliserExc :: Zero -> MonMemory -> User sig Zero a
monFinaliserExc e _ = impossible e

monFinaliserSig :: MonS -> User sig Zero a
monFinaliserSig s = error ("signal reached (monotonic) top level (" ++ show s ++ ")")

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
