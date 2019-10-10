{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

--
-- Simple integer-valued ML-style state implemented using runners,  
-- supporting allocation lookup, and assignment of memory references.
--

module Control.Runner.IntMLState
  (
  Ref, IntMLState,
  alloc, (!), (=:=),
  intMlRunner, intMlInitialiser, intMlFinaliser, intMlTopLevel,
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
-- Typed integer-valued references.
--
type Addr = Nat

data Ref where
  R :: Addr -> Ref

mkRef :: Addr -> Ref
mkRef addr = R addr

addrOf :: Ref -> Addr
addrOf (R r) = r

--
-- Type of heaps, with associated select, update, and alloc functions.
--
type Memory = Ref -> Maybe Int

data Heap = H { memory :: Memory, nextAddr :: Addr }

heapSel :: Heap -> Ref -> Int
heapSel h r =
  case memory h r of
    Nothing -> error "reference not in the heap"
    Just x -> x

memUpd :: Memory -> Ref -> Int -> Memory
memUpd m r x r' =
  if (addrOf r == addrOf r')
  then Just x
  else m r'

heapUpd :: Heap -> Ref -> Int -> Heap
heapUpd h r x = h { memory = memUpd (memory h) r x }

heapAlloc :: Heap -> Int -> (Ref,Heap)
heapAlloc h init =
  let r = mkRef (nextAddr h) in 
  (r , H { memory = memUpd (memory h) r init ,
           nextAddr = S (nextAddr h) })

--
-- Signature of ML-style integer-valued state operations.
--
data IntMLState :: * -> * where
  Alloc  :: Int -> IntMLState Ref
  Deref  :: Ref -> IntMLState Int
  Assign :: Ref -> Int -> IntMLState ()

--
-- Generic effects.
--
alloc :: Member IntMLState sig => Int -> User sig Ref
alloc init = focus (performU (Alloc init))

(!) :: Member IntMLState sig => Ref -> User sig Int
(!) r = focus (performU (Deref r))

(=:=) :: Member IntMLState sig => Ref -> Int -> User sig ()
(=:=) r x = focus (performU (Assign r x))

--
-- ML-style integer-valued memory runner.
--
intMlCoOps :: IntMLState a -> Kernel sig Heap a
intMlCoOps (Alloc init) =
  do h <- getEnv;
     (r,h') <- return (heapAlloc h init);
     setEnv h';
     return r
intMlCoOps (Deref r)    =
  do h <- getEnv;
     return (heapSel h r)
intMlCoOps (Assign r x) =
  do h <- getEnv;
     setEnv (heapUpd h r x)

intMlRunner :: Runner '[IntMLState] sig Heap
intMlRunner = mkRunner intMlCoOps

--
-- Top-Level running of the ML-style memory.
--
intMlInitialiser :: User sig Heap
intMlInitialiser = return (H { memory = \ _ -> Nothing , nextAddr = Z })

intMlFinaliser :: a -> Heap -> User sig a
intMlFinaliser x _ = return x

intMlTopLevel :: User '[IntMLState] a -> a
intMlTopLevel m =
  pureTopLevel (
    run
      intMlRunner
      intMlInitialiser
      m
      intMlFinaliser
  )