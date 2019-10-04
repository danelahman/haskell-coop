{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

--
-- Koka-style ambient values and ambient functions implemented using runners.
--

module Control.Runner.Ambients
  (
  AmbFun, AmbVal, Amb, AmbEff, 
  getVal, applyFun,
  rebindVal, rebindFun,
  withAmbVal, withAmbFun,
  ambTopLevel
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
-- Ambient functions store a reference to a function in the heap.
--
type Addr = Nat

data AmbFun a b where
  F :: (Typeable a,Typeable b) => Addr -> AmbFun a b

mkAmb :: (Typeable a,Typeable b) => Addr -> AmbFun a b
mkAmb addr = F addr

addrOf :: AmbFun a b -> Addr
addrOf (F r) = r

--
-- Ambient values as a special case of ambient functions.
--
data AmbVal a where
  AV :: (Typeable a) => AmbFun () a -> AmbVal a

--
-- Sugar for user computations with the ambient effect. 
--
type AmbEff a = User '[Amb] a

--
-- Ambient memory optionally maps an ambient functions to
-- their values at each depth of the callstack. Ambient
-- heap stores the memory, address of the next ambient 
-- function, and the current depth of the callstack.
--
type Depth = Nat

type AmbMemory =
  forall a b sig .
    (Typeable a,Typeable b) =>
      AmbFun a b -> Depth -> Maybe (a -> AmbEff b)

data AmbHeap =
  H { memory :: AmbMemory, nextAddr :: Addr, depth :: Depth }

ambHeapSel :: (Typeable a,Typeable b)
           => AmbHeap
           -> AmbFun a b
           -> Depth
           -> (a -> AmbEff b,Depth)
ambHeapSel h f Z =
  case memory h f Z of
    Nothing -> error "Ambient function not bound"
    Just f -> (f,Z)
ambHeapSel h f (S d) =
  case memory h f (S d) of
    Nothing -> ambHeapSel h f d
    Just f -> (f,S d)

ambMemUpd :: (Typeable a,Typeable b)
          => AmbMemory
          -> AmbFun a b
          -> (a -> AmbEff b)
          -> Depth
          -> AmbMemory
ambMemUpd mem f g d f' d' =
  case cast g of
    Nothing -> mem f' d'
    Just g -> (
      if (addrOf f == addrOf f' && d == d')
      then Just g
      else mem f' d')

ambHeapUpd :: (Typeable a,Typeable b)
           => AmbHeap
           -> AmbFun a b
           -> (a -> AmbEff b)
           -> AmbHeap
ambHeapUpd h f g =
  h { memory = ambMemUpd (memory h) f g (depth h) ,
      depth = S (depth h) }

ambHeapAlloc :: (Typeable a,Typeable b)
             => AmbHeap
             -> (a -> AmbEff b)
             -> (AmbFun a b,AmbHeap)
ambHeapAlloc h f =
  let addr = nextAddr h in
  let g = mkAmb addr in
  let d = depth h in
  (g , H { memory = ambMemUpd (memory h) g f d ,
           nextAddr = S addr ,
           depth = S d })

--
-- Ambient effect, allowing to bind, apply, and rebind ambient functions.
--
data Amb :: * -> * where
  Bind   :: (Typeable a,Typeable b) => (a -> AmbEff b) -> Amb (AmbFun a b)
  Apply  :: (Typeable a,Typeable b) => AmbFun a b -> a -> Amb b
  Rebind :: (Typeable a,Typeable b) => AmbFun a b -> (a -> AmbEff b) -> Amb ()
  
--
-- Public generic effects.
--
getVal :: (Typeable a) => AmbVal a -> AmbEff a
getVal (AV x) = focus (performU (Apply x ()))

applyFun :: (Typeable a,Typeable b) => AmbFun a b -> a -> AmbEff b
applyFun f x = focus (performU (Apply f x))

rebindVal :: (Typeable a) => AmbVal a -> a -> AmbEff ()
rebindVal (AV x) y = focus (performU (Rebind x (\ _ -> return y)))

rebindFun :: (Typeable a,Typeable b)
          => AmbFun a b
          -> (a -> AmbEff b)
          -> AmbEff ()
rebindFun f g = focus (performU (Rebind f g))

--
-- Private generic effect.
--
bind :: (Typeable a,Typeable b)
     => (a -> AmbEff b)
     -> AmbEff (AmbFun a b)
bind f = focus (performU (Bind f))

--
-- Runner for ambient functions.
--
-- Ambient function application finds the ambient
-- function in the heap, and runs the application
-- at the corresponding depth of the callstack.
--
ambCoOps :: Amb a -> Kernel sig AmbHeap a
ambCoOps (Bind f) =
  do h <- getEnv;
     (f,h') <- return (ambHeapAlloc h f);
     setEnv h';
     return f
ambCoOps (Apply f x) =
  do h <- getEnv;
     (f,d) <- return (ambHeapSel h f (depth h));
     execK
       (run
          ambRunner
          (return (h {depth = d}))
          (f x)
          ambFinaliser)
       return
ambCoOps (Rebind f g) =
  do h <- getEnv;
     setEnv (ambHeapUpd h f g)

ambRunner :: Runner '[Amb] sig AmbHeap
ambRunner = mkRunner ambCoOps

--
-- Scoped (initial) binding of ambient values.
--
withAmbVal :: (Typeable a)
           => a
           -> (AmbVal a -> AmbEff b) -> AmbEff b
withAmbVal x k =
  do f <- bind (\ _ -> return x);
     k (AV f)

--
-- Scoped (initial) binding of ambient functions.
--
withAmbFun :: (Typeable a,Typeable b)
           => (a -> AmbEff b)
           -> (AmbFun a b -> AmbEff c) -> AmbEff c
withAmbFun f k =
  do f <- bind f;
     k f

--
-- Top-level for running ambient functions.
--
ambInitialiser :: User sig AmbHeap
ambInitialiser =
  return (H { memory = \ _ _ -> Nothing ,
              nextAddr = Z ,
              depth = Z })

ambFinaliser :: a -> AmbHeap -> User sig a
ambFinaliser x _ = return x

ambTopLevel :: AmbEff a -> a
ambTopLevel m =
  pureTopLevel (
    run
      ambRunner
      ambInitialiser
      m
      ambFinaliser
  )