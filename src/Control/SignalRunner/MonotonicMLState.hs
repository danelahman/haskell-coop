{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

--
-- ...
--

module Control.SignalRunner.MonotonicMLState
  (
--  Ref, MLState, S(..), 
--  alloc, (!), (=:=),
--  mlRunner, mlInitialiser, mlFinaliserVal, mlFinaliserExc, mlFinaliserSig, mlTopLevel,
--  Typeable
  ) where

import Control.SignalRunner
import Control.SignalRunner.SignalMLState

import Data.Typeable

--
-- Kill signal(s).
--
data MonS where
  MLStateSignal :: S -> MonS
  MononicityViolationSignal :: Ref a -> MonS

instance Show MonS where
  show (MLStateSignal s) = show s
  show (MononicityViolationSignal r) = "MononicityViolationSignal -- " ++ show r

--
-- Type of preorders (omitting the reflexicity-transitivity constraints).
--
type Preorder a = a -> a -> Bool

--
-- Type of memory storing monotonicity preorders for references.
--
--
type MonMemory = forall a . (Typeable a) => Ref a -> Maybe (Preorder a)

memSel :: (Typeable a) => MonMemory -> Ref a -> Maybe (Preorder a)
memSel mem r = mem r

memUpd :: (Typeable a) => MonMemory -> Ref a -> Preorder a -> MonMemory
memUpd mem r p r' =
  case cast p of
    Nothing -> mem r'
    Just q -> (
      if (addrOf r == addrOf r')
      then Just q
      else mem r')



{-           
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
alloc :: (Typeable a,Member MLState sig) => a -> User sig e (Ref a)
alloc init = tryWithU (focus (performU (Alloc init))) return impossible

(!) :: (Typeable a,Member MLState sig) => Ref a -> User sig e a
(!) r = tryWithU (focus (performU (Deref r))) return impossible

(=:=) :: (Typeable a,Member MLState sig) => Ref a -> a -> User sig e ()
(=:=) r x = tryWithU (focus (performU (Assign r x))) return impossible

--
-- ML-style memory runner.
--
mlCoOps :: MLState a -> Kernel sig Zero S Heap a
mlCoOps (Alloc init) =
  do h <- getEnv;
     (r,h') <- return (heapAlloc h init);
     setEnv h';
     return r
mlCoOps (Deref r)    =
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

mlRunner :: Runner '[MLState] sig S Heap
mlRunner = mkRunner mlCoOps

--
-- Top-Level running of the ML-style memory.
--
mlInitialiser :: User sig Zero Heap
mlInitialiser = return (H { memory = \ _ -> Nothing , nextAddr = Z })

mlFinaliserVal :: a -> Heap -> User sig Zero a
mlFinaliserVal x _ = return x

mlFinaliserExc :: Zero -> Heap -> User sig Zero a
mlFinaliserExc e _ = impossible e

mlFinaliserSig :: S -> User sig Zero a
mlFinaliserSig s = error ("signal reached top level (" ++ show s ++ ")")

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
-}