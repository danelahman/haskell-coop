{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Runner.Ambients (
  Nat,
  AmbFun, AmbVal, Amb, AmbEff, 
  getVal, applyFun,
  rebindVal, rebindFun,
  withAmbVal, withAmbFun,
  ambTopLevel
  ) where

import Control.Monad.Runner

import Data.Typeable
import System.IO

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
--
--
data AmbFun a b where
  F :: (Typeable a,Typeable b) => Nat -> AmbFun a b

type AmbVal a = (Typeable a) => AmbFun () a

type AmbEff a = User '[Amb] a

mkAmb :: (Typeable a,Typeable b) => Nat -> AmbFun a b
mkAmb addr = F addr

addr_of :: AmbFun a b -> Nat
addr_of (F r) = r

--
-- 
--
type Stage = Nat

type AmbMemory =
  forall a b iface . (Typeable a,Typeable b) => AmbFun a b -> Stage -> Maybe (a -> AmbEff b)

data AmbHeap =
  H { memory :: AmbMemory, nextAddr :: Nat, stage :: Stage }

ambHeapSel :: (Typeable a,Typeable b) => AmbHeap -> AmbFun a b -> Stage -> (a -> AmbEff b,Stage)
ambHeapSel h f Z =
  case memory h f Z of
    Nothing -> error "Ambient function not bound"
    Just f -> (f,Z)
ambHeapSel h f (S s) =
  case memory h f (S s) of
    Nothing -> ambHeapSel h f s
    Just f -> (f,S s)

ambMemUpd :: (Typeable a,Typeable b) => AmbMemory -> AmbFun a b -> (a -> AmbEff b) -> Stage -> AmbMemory
ambMemUpd mem f g s f' s' =
  case cast g of
    Nothing -> mem f' s'
    Just g -> (
      if (addr_of f == addr_of f' && s == s')
      then Just g
      else mem f' s')

ambHeapUpd :: (Typeable a,Typeable b) => AmbHeap -> AmbFun a b -> (a -> AmbEff b) -> AmbHeap
ambHeapUpd h f g = h { memory = ambMemUpd (memory h) f g (stage h) , stage = S (stage h) }

ambHeapAlloc :: (Typeable a,Typeable b) => AmbHeap -> (a -> AmbEff b) -> (AmbFun a b,AmbHeap)
ambHeapAlloc h f =
  let addr = nextAddr h in
  let g = mkAmb addr in
  let s = stage h in
  (g , H { memory = ambMemUpd (memory h) g f s ,
           nextAddr = S addr ,
           stage = S s })

--
--
--
data Amb :: * -> * where
  Bind  :: (Typeable a,Typeable b) => (a -> AmbEff b) -> Amb (AmbFun a b)
  Apply  :: (Typeable a,Typeable b) => AmbFun a b -> a -> Amb b
  Rebind :: (Typeable a,Typeable b) => AmbFun a b -> (a -> AmbEff b) -> Amb ()
  
--
-- Public generic effects.
--
getVal :: (Typeable a) => AmbVal a -> AmbEff a
getVal x = focus (performU (Apply x ()))

applyFun :: (Typeable a,Typeable b) => AmbFun a b -> a -> AmbEff b
applyFun f x = focus (performU (Apply f x))

rebindVal :: (Typeable a) => AmbVal a -> a -> AmbEff ()
rebindVal x y = focus (performU (Rebind x (\ _ -> return y)))

rebindFun :: (Typeable a,Typeable b) => AmbFun a b -> (a -> AmbEff b) -> AmbEff ()
rebindFun f g = focus (performU (Rebind f g))

--
-- Private generic effect.
--
bind :: (Typeable a,Typeable b) => (a -> AmbEff b) -> AmbEff (AmbFun a b)
bind f = focus (performU (Bind f))

--
--
--
ambCoOps :: Amb a -> Kernel iface AmbHeap a
ambCoOps (Bind f) =
  do h <- getEnv;
     (f,h') <- return (ambHeapAlloc h f);
     setEnv h';
     return f
ambCoOps (Apply f x) =
  do h <- getEnv;
     (f,s) <- return (ambHeapSel h f (stage h));
     execK (run ambRunner (return (h {stage = s})) (f x) ambFinaliser) return
ambCoOps (Rebind f g) =
  do h <- getEnv;
     setEnv (ambHeapUpd h f g)

ambRunner :: Runner '[Amb] iface AmbHeap
ambRunner = mkRunner ambCoOps

--
--
--
withAmbVal :: (Typeable a)
           => a
           -> (AmbVal a -> AmbEff b) -> AmbEff b
withAmbVal x k =
  do f <- bind (\ _ -> return x);
     k f

withAmbFun :: (Typeable a,Typeable b)
           => (a -> AmbEff b)
           -> (AmbFun a b -> AmbEff c) -> AmbEff c
withAmbFun f k =
  do f <- bind f;
     k f

--
--
--
ambInitialiser :: User iface AmbHeap
ambInitialiser = return (H { memory = \ _ _ -> Nothing , nextAddr = Z , stage = Z })

ambFinaliser :: a -> AmbHeap -> User iface a
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