{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-
module Control.Monad.Runner.Ambients (
  Nat,
  AmbFun, AmbVal, Amb,
  get, apply,
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
  F :: (Typeable a,Typeable b) => Nat -> (a -> b) -> AmbFun a b

type AmbVal a = (Typeable a) => AmbFun () a

mkAmb :: (Typeable a,Typeable b) => Nat -> (a -> b) -> AmbFun a b
mkAmb addr x = F addr x

addr_of :: AmbFun a b -> Nat
addr_of (F r _) = r

initial :: AmbFun a b -> a -> b
initial (F _ f) = f

--
--
--
type AmbMemory =
  forall a b . (Typeable a,Typeable b) => AmbFun a b -> Maybe (a -> b)


data AmbHeap =
  H { memory :: AmbMemory, next_addr :: Nat }

ambHeapSel :: (Typeable a,Typeable b) => AmbHeap -> AmbFun a b -> a -> b
ambHeapSel h f =
  case memory h f of
    Nothing -> initial f
    Just f -> f

ambMemUpd :: (Typeable a,Typeable b) => AmbMemory -> AmbFun a b -> (a -> b) -> AmbMemory
ambMemUpd mem f g f' =
  case cast g of
    Nothing -> mem f'
    Just g -> (
      if addr_of f == addr_of f'
      then Just g
      else mem f')


ambHeapUpd :: (Typeable a,Typeable b) => AmbHeap -> AmbFun a b -> (a -> b) -> AmbHeap
ambHeapUpd h f g = h { memory = ambMemUpd (memory h) f g }

ambHeapAlloc :: (Typeable a,Typeable b) => AmbHeap -> (a -> b) -> (AmbFun a b,AmbHeap)
ambHeapAlloc h f =
  let g = mkAmb (next_addr h) f in 
  (g , H { memory = ambMemUpd (memory h) g f ,
           next_addr = S (next_addr h) })

--
--
--
data Amb :: * -> * where
  Bind  :: (Typeable a,Typeable b) => (a -> b) -> Amb (AmbFun a b)
  Apply  :: (Typeable a,Typeable b) => AmbFun a b -> a -> Amb b
  Rebind :: (Typeable a,Typeable b) => AmbFun a b -> (a -> b) -> Amb ()

--
-- Public generic effects.
--
get :: (Typeable a,Member Amb iface) => AmbVal a -> User iface a
get x = focus (performU (Apply x ()))

apply :: (Typeable a,Typeable b,Member Amb iface) => AmbFun a b -> a -> User iface b
apply f x = focus (performU (Apply f x))

rebindVal :: (Typeable a,Member Amb iface) => AmbVal a -> a -> User iface ()
rebindVal x y = focus (performU (Rebind x (\ _ -> y)))

rebindFun :: (Typeable a,Typeable b,Member Amb iface) => AmbFun a b -> (a -> b) -> User iface ()
rebindFun f g = focus (performU (Rebind f g))

--
-- Private generic effect.
--
bind :: (Typeable a,Typeable b,Member Amb iface) => (a -> b) -> User iface (AmbFun a b)
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
     f <- return (ambHeapSel h f);
     return (f x)
ambCoOps (Rebind f g) =
  do h <- getEnv;
     setEnv (ambHeapUpd h f g)

ambRunner :: Runner '[Amb] iface AmbHeap
ambRunner = mkRunner ambCoOps

--
--
--
withAmbVal :: (Typeable a,Member Amb iface)
           => a
           -> (AmbVal a -> User iface b) -> User iface b
withAmbVal x k =
  do f <- bind (\ _ -> x);
     k f

withAmbFun :: (Typeable a,Typeable b,Member Amb iface)
           => (a -> b)
           -> (AmbFun a b -> User iface c) -> User iface c
withAmbFun f k =
  do f <- bind f;
     k f


--
--
--
ambInitialiser :: User iface AmbHeap
ambInitialiser = return (H { memory = \ _ -> Nothing , next_addr = Z })

ambFinaliser :: a -> AmbHeap -> User iface a
ambFinaliser x _ = return x

ambTopLevel :: User '[Amb] a -> a
ambTopLevel m =
  pureTopLevel (
    run
      ambRunner
      ambInitialiser
      m
      ambFinaliser
  )
-}