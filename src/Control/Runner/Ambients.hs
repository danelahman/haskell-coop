{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Control.Ambients
Description : Runner for Koka-style ambient values and ambient functions
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides a runner that implements ambient values and ambient 
functions as present in the [Koka](https://github.com/koka-lang/koka) 
language. Ambient values are essentially just mutable variables. Ambient 
functions are functions that are dynamically bound but evaluated in the 
lexical scope of their binding.

As a simple example, in the following code snippet (in Koka-style syntax)

> ambient fun f : int -> int
> ambient val x : int
>
> with val x = 4
> with fun f = fun y -> x + y
> with val x = 2
> f 1

the final application results in @5@ instead of @3@ that one might 
naively expect. This is so because @f@ is an ambient function, 
and thus the application happens in the context in which it was 
last bound, where the ambient value @x@ still had the value @4@.

We implement ambient values and ambient functions using a runner by 
treating the binding and application operations as co-operations. 
Internally, the runner carries a heap of ambient values and ambient 
functions, where each address tracks the values of these at different 
points in time (i.e., at the points when new values and functions get 
bound, or old ones rebound). The application co-operation then temporarily 
rewinds the heap to the state where the function was last bound, 
evaluates the application in that state, and then restores the state 
of the heap as it was before the ambient function application.

For more details about ambient values and ambient functions, how they 
can be used in practice and operationally implemented, we suggest 
this [technical report](https://www.microsoft.com/en-us/research/uploads/prod/2019/03/implicits-tr-v2.pdf).
-}
module Control.Runner.Ambients
  (
  AmbFun, AmbVal, Amb(..), AmbEff, 
  getVal, applyFun,
  rebindVal, rebindFun,
  ambRunner, AmbHeap,
  withAmbVal, withAmbFun,
  ambTopLevel
  ) where

import Control.Runner

import Data.Typeable

-- | Type of natural numbers (used for memory addresses).
data Nat where
  Z :: Nat
  S :: Nat -> Nat

instance Eq Nat where
  Z == Z = True
  (S n) == (S m) = n == m
  _ == _ = False

-- | Addresses of ambient values and functions.
type Addr = Nat

-- | Type of ambient functions, from type @a@ to type @b@.
data AmbFun a b where
  F :: (Typeable a,Typeable b) => Addr -> AmbFun a b

-- | The memory address of an ambient function.
addrOf :: AmbFun a b -> Addr
addrOf (F r) = r

-- | Type of ambient values of type @a@.
data AmbVal a where
  AV :: (Typeable a) => AmbFun () a -> AmbVal a

-- | Depth of the state of the heap, in terms of the number of
-- bindings and rebindings of ambient values and ambient functions
-- that have happened in the past.
type Depth = Nat

-- | Memory is a partial depth-dependent mapping from ambient values
-- and ambient functions to correspondingly typed `AmbEff` functions. 
type AmbMemory =
  forall a b sig .
    (Typeable a,Typeable b) =>
      AmbFun a b -> Depth -> Maybe (a -> AmbEff b)

-- | Heap in which we store the definitions of ambient values and ambient functions.
data AmbHeap =
  H { memory   :: AmbMemory, -- memory that stores definitions of ambient values and ambient functions
      nextAddr :: Addr,      -- address of the next ambient value or ambient function to be allocated
      depth    :: Depth }    -- the current binding depth of the heap

-- | Selecting an ambient function definition from the heap. Also returns
-- the depth at which the ambient function was bound to its definition.
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

-- | Updating the memory with a new ambient value or ambient function definition.
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

-- | Updating the heap with a new ambient value or ambient function definition.
ambHeapUpd :: (Typeable a,Typeable b)
           => AmbHeap
           -> AmbFun a b
           -> (a -> AmbEff b)
           -> AmbHeap
ambHeapUpd h f g =
  h { memory = ambMemUpd (memory h) f g (depth h) ,
      depth = S (depth h) }

-- | Allocating a new ambient value or ambient function in the heap.
ambHeapAlloc :: (Typeable a,Typeable b)
             => AmbHeap
             -> (a -> AmbEff b)
             -> (AmbFun a b,AmbHeap)
ambHeapAlloc h f =
  let addr = nextAddr h in
  let g = F addr in
  let d = depth h in
  (g , H { memory = ambMemUpd (memory h) g f d ,
           nextAddr = S addr ,
           depth = S d })

-- | The effect for programming with ambient values and ambient functions.
data Amb :: * -> * where
  -- | Algebraic operation for (the initial) binding of a value to an ambient value.
  BindVal   :: (Typeable a) => a -> Amb (AmbVal a)
  -- | Algebraic operation for (the initial) binding of a function to an ambient function.
  BindFun   :: (Typeable a,Typeable b) => (a -> AmbEff b) -> Amb (AmbFun a b)
  -- | Algebraic operation for getting the value of an ambient value.
  GetVal    :: (Typeable a) => AmbVal a -> Amb a
  -- | Algebraic operation for applying an ambient function to a value.
  ApplyFun  :: (Typeable a,Typeable b) => AmbFun a b -> a -> Amb b
  -- | Algebraic operation for rebinding an ambient value to a new value.
  RebindVal :: (Typeable a) => AmbVal a -> a -> Amb ()
  -- | Algebraic operation for rebinding an ambient function to a new function.
  RebindFun :: (Typeable a,Typeable b) => AmbFun a b -> (a -> AmbEff b) -> Amb ()

-- | Syntactic sugar for the type of user computations that can perform the `Amb` effect. 
type AmbEff a = User '[Amb] a

-- | Generic effect for getting the value of an ambient value.
getVal :: (Typeable a) => AmbVal a -> AmbEff a
getVal (AV x) = focus (performU (ApplyFun x ()))

-- | Generic effect for applying an ambient function to a value.
applyFun :: (Typeable a,Typeable b) => AmbFun a b -> a -> AmbEff b
applyFun f x = focus (performU (ApplyFun f x))

-- | Generic effect for rebinding an ambient value to a new value.
rebindVal :: (Typeable a) => AmbVal a -> a -> AmbEff ()
rebindVal x y = focus (performU (RebindVal x y))

-- | Generic effect for rebinding an ambient function to a new function.
rebindFun :: (Typeable a,Typeable b)
          => AmbFun a b
          -> (a -> AmbEff b)
          -> AmbEff ()
rebindFun f g = focus (performU (RebindFun f g))

-- | Generic effect for (the initial) binding of an ambient value.
bindVal :: Typeable a
        => a
        -> AmbEff (AmbVal a)
bindVal x = focus (performU (BindVal x))

-- | Generic effect for (the initial) binding of an ambient function.
bindFun :: (Typeable a,Typeable b)
        => (a -> AmbEff b)
        -> AmbEff (AmbFun a b)
bindFun f = focus (performU (BindFun f))

-- | Co-operations of the runner implementing ambient values and ambient functions.
ambCoOps :: Amb a -> Kernel sig AmbHeap a
ambCoOps (BindVal x) =
  do h <- getEnv;
     (x,h') <- return (ambHeapAlloc h (\ _ -> return x));
     setEnv h';
     return (AV x)
ambCoOps (BindFun f) =
  do h <- getEnv;
     (f,h') <- return (ambHeapAlloc h f);
     setEnv h';
     return f
ambCoOps (GetVal (AV x)) =
  do h <- getEnv;
     (x,d) <- return (ambHeapSel h x (depth h));
     user
       (run
          ambRunner
          (return h)
          (x ())
          ambFinaliser)
       return
ambCoOps (ApplyFun f x) =
  do h <- getEnv;
     (f,d) <- return (ambHeapSel h f (depth h));
     user
       (run
          ambRunner
          (return (h {depth = d})) -- observe that we rewind the heap to the poin where @f@ was last bound
          (f x)
          ambFinaliser)
       return
ambCoOps (RebindVal (AV x) y) =
  do h <- getEnv;
     setEnv (ambHeapUpd h x (\ _ -> return y))
ambCoOps (RebindFun f g) =
  do h <- getEnv;
     setEnv (ambHeapUpd h f g)

-- | Runner implementing ambient values and ambient functions.
ambRunner :: Runner '[Amb] sig AmbHeap
ambRunner = mkRunner ambCoOps

-- | Scoped initial binding of an ambient value.
withAmbVal :: (Typeable a)
           => a
           -> (AmbVal a -> AmbEff b) -> AmbEff b
withAmbVal x k =
  do x <- bindVal x;
     k x

-- | Scoped initial binding of an ambient function.
withAmbFun :: (Typeable a,Typeable b)
           => (a -> AmbEff b)
           -> (AmbFun a b -> AmbEff c) -> AmbEff c
withAmbFun f k =
  do f <- bindFun f;
     k f

-- | Initialiser for running user code with `ambRunner`.
ambInitialiser :: User sig AmbHeap
ambInitialiser =
  return (H { memory = \ _ _ -> Nothing ,
              nextAddr = Z ,
              depth = Z })

-- | Finaliser for running user code with `ambRunner`.
ambFinaliser :: a -> AmbHeap -> User sig a
ambFinaliser x _ = return x

-- | Top level for running user code with the `AmbEff` effect.
--
-- Internally this top level functionality runs the user code
-- using the runner `ambRunner`.
ambTopLevel :: AmbEff a -> a
ambTopLevel m =
  pureTopLevel (
    run
      ambRunner
      ambInitialiser
      m
      ambFinaliser
  )