{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Runner.Ambients where

import Control.Monad.Runner
import System.IO

--
-- Encoding Koka's ambient functions (and ambient values) as a runner.
--

--
-- Datatypes of natural numbers (for number of bound ambient functions).
--
data Nat where
  Z :: Nat
  S :: Nat -> Nat

--
-- Shape of the bound ambients (types of their values) of a given size.
--
data AmbShape :: Nat -> * where
  ShE :: AmbShape Z
  ShC :: * -> * -> AmbShape n -> AmbShape (S n)
--
-- Values of bound ambients corresponding to a shape.
--
data AmbFun :: forall ambsize . AmbShape ambsize -> * where
  AE :: AmbFun ShE
  AC :: (a -> b) -> AmbFun sh -> AmbFun (ShC a b sh)

--
-- Datatype of addresses of bound ambients.
--
data Addr a b :: forall ambsize . AmbShape ambsize -> * where
  AZ :: Addr a b (ShC a b sh)
  AS :: Addr a b sh -> Addr a b (ShC c d sh)

--
-- Looking up the value of a bound ambient (function).
--
lkp :: AmbFun sh -> Addr a b sh -> a -> b
lkp (AC f _) AZ = f
lkp (AC _ fs) (AS addr) = lkp fs addr

upd :: AmbFun sh -> Addr a b sh -> (a -> b) -> AmbFun sh
upd (AC _ fs) AZ f = AC f fs
upd (AC g fs) (AS addr) f = AC g (upd fs addr f)

--
-- Signature of the ambient functions effect for 0..m-1 bound ambient functions.
--
data Amb (sh :: AmbShape ambsize) :: * -> * where
  Apply :: Addr a b ambshape -> a -> Amb ambshape b

--
-- Generic effect for applying an ambient functions.
--
apply :: Addr a b sh -> a -> User '[Amb sh] b
apply addr x = performU (Apply addr x)

--
-- Generic effect for getting the value of an ambient value.
--
get :: Addr () a sh -> User '[Amb sh] a
get addr = performU (Apply addr ())

--
-- Runner for ambient functions.
--
ambCoOps :: Amb sh a -> Kernel iface (AmbFun sh) a
ambCoOps (Apply f x) =
  do fs <- getEnv;
     return ((lkp fs f) x)

ambRunner :: Runner '[Amb sh] iface (AmbFun sh)
ambRunner = mkRunner ambCoOps

--
-- With-amb-fun construct
--
ambInitialiser :: (a -> b) -> User '[Amb sh] (AmbFun (ShC a b sh))
ambInitialiser f = error "."

