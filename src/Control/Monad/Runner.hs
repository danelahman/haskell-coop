{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Runner (
  User, Kernel,
  performU, performK,
  getEnv, setEnv,
  Runner, mkRunner, emptyRunner, unionRunners, pairRunners,
  run, execU, execK,
  topLevel, pureTopLevel, ioTopLevel,
  embedU, embedK, embedRunner, extendRunner, focus
  ) where

--
-- Tested with
--   http://hackage.haskell.org/package/freer-simple [v1.2.1.0]
-- variant of the Freer monad.
--
import Control.Monad.Freer.Internal hiding (run)

--
-- User monad modelling user computations waiting to be run
-- by some enveloping runner. We define this monad on top of
-- the Freer monad, and analogously iface is a list of
-- effects, each represented by a type of kind `* -> *`.
--
-- The type `User iface a` captures the typing judgement
--
--   Gamma  |-^iface  M  :  a
--
newtype User iface a =
  UC (Eff iface a) deriving (Functor,Applicative,Monad)

--
-- Kernel monad modelling kernel computations that can trigger
-- effects in `iface` and access runtime state of type `c`.
-- We use kernel computations to implement co-operations.
--
newtype Kernel iface s a =
  KC (s -> Eff iface (a,s)) deriving (Functor)

instance Applicative (Kernel iface c) where
  pure v = KC (\ c -> return (v,c))
  (KC f) <*> (KC k) =
    KC (\ c -> do (g,c') <- f c;
                  (x,c'') <- k c';
                  return (g x,c''))

instance Monad (Kernel iface c) where
  return x = KC (\ c -> return (x,c))
  (KC k) >>= f =
    KC (\ c -> do (x,c') <- k c;
                  let (KC l) = f x in l c')

--
-- Performing user operations while focussed on a single effect.
--
-- If working with a bigger interface, first focus on a single
-- effect before calling perform. This is a small price to
-- pay to get more intricate GADT-based interfaces (such as
-- various kinds of state) to work conveniently. In most cases
-- focussing is limited to defining derived generic effects.
--
performU :: e r -> User '[e] r
performU o = UC (send o)

--
-- Generic user perform function used internally in this module.
--
genPerformU :: Member e iface => e r -> User iface r
genPerformU o = UC (send o)

--
-- Performing kernel operations while focussed on a single effect.
--
performK :: e r -> Kernel '[e] c r
performK o = KC (\ c -> do x <- send o; return (x,c))

--
-- Generic kernel perform function used internally in this module.
--
genPerformK :: Member e iface => e r -> Kernel iface c r
genPerformK o = KC (\ c -> do x <- send o; return (x,c))

--
-- State-access operations for the kernel monad.
--
getEnv :: Kernel iface c c
getEnv = KC (\ c -> return (c,c))

setEnv :: c -> Kernel iface c ()
setEnv c' = KC (\ c -> return ((),c'))

--
-- Executing a kernel computation inside user computations.
--
execU :: Kernel iface c a -> c -> (a -> c -> User iface b) -> User iface b
execU (KC k) c f =
  UC (do (x,c') <- k c; let (UC m) = f x c' in m)

--
-- Executing a user computation inside kernel computations.
--
execK :: User iface a -> (a -> Kernel iface c b) -> Kernel iface c b
execK (UC m) f =
  KC (\ c -> do x <- m; let (KC k) = f x in k c)

--
-- A runner is simply a list of co-operations, where the first
-- argument of CoOps requires a co-operation for each operation
-- in the given effect `e :: * -> *`, e.g., for `FRead` and `FWrite`.
--
data Runner iface iface' c where
  Empty :: Runner '[] iface' c
  CoOps :: (forall b . e b -> Kernel iface' c b)
        -> Runner iface iface' c-> Runner (e ': iface) iface' c

mkRunner :: (forall b . e b -> Kernel iface c b) -> Runner '[e] iface c
mkRunner coops = CoOps coops Empty

--
-- The
--
--  using C @ m_init
--  run m
--  finally { return x @ c -> m_fin }
--
-- construct for initialising, running, and finalising a user computation.
--
runOp :: Runner iface iface' c -> Union iface b -> Kernel iface' c b
runOp Empty _ =
  error "this should not have happened"
runOp (CoOps coop coops) u =
  case decomp u of
    Right o -> coop o
    Left u -> runOp coops u

runU :: Runner iface iface' c
     -> c
     -> User iface a
     -> (a -> c -> User iface' b)
     -> User iface' b
runU r c (UC (Val x)) mf = mf x c
runU r c (UC (E u q)) mf =
  execU (runOp r u) c
        (\ x c' -> runU r c' (UC (qApp q x)) mf)

run :: Runner iface iface' c
    -> User iface' c
    -> User iface a
    -> (a -> c -> User iface' b)
    -> User iface' b
run r mi m mf =
  do c <- mi; runU r c m mf

--
-- Running a user computation in a top-level container (monad).
--
--
topLevel :: Monad m => User '[m] a -> m a
topLevel (UC c) = runM c

--
-- Running user computation in a top-level pure containers.
--
pureTopLevel :: User '[] a -> a
pureTopLevel (UC (Val x)) = x
pureTopLevel _ = error "this should not have happened"

--
-- Short-hand for running user computation ina top-level IO container.
--
ioTopLevel :: User '[IO] a -> IO a
ioTopLevel = topLevel

--
-- Empty runner and union of runners.
--
emptyRunner :: Runner '[] iface' c
emptyRunner = Empty

type family IfaceUnion (iface :: [* -> *]) (iface' :: [* -> *]) :: [* -> *] where 
  IfaceUnion '[] iface' = iface'
  IfaceUnion (e ': iface) iface' = e ': (IfaceUnion iface iface')

unionRunners :: Runner iface iface'' c -> Runner iface' iface'' c
              -> Runner (IfaceUnion iface iface') iface'' c
unionRunners Empty r' = r'
unionRunners (CoOps coops r) r' =
  CoOps coops (unionRunners r r')

--
-- Embedding a user computations in a larger signature of operations.
--
embedU :: User iface a -> User (e ': iface) a
embedU (UC m) = UC (raise m)

--
-- Embedding a kernel computations in a larger signature of operations.
--
embedK :: Kernel iface c a -> Kernel (e ': iface) c a
embedK (KC k) = KC (\ c -> do (x,c') <- raise (k c);
                              return (x,c'))

--
-- Embedding a runner in a larger signature of operations.
--
embedRunner :: Runner iface iface' c -> Runner iface (e ': iface') c
embedRunner Empty = Empty
embedRunner (CoOps coops r) =
  CoOps (\ o -> embedK (coops o)) (embedRunner r)

--
-- Extending the carrier of a runner with some type/set.
--
extendRunner :: Runner iface iface' c' -> Runner iface iface' (c,c')
extendRunner Empty = Empty
extendRunner (CoOps coops r) =
  CoOps (\ o -> KC (\ (c,c') ->
                  do (x,c'') <- let (KC k) = coops o in k c';
                     return (x,(c,c''))))
        (extendRunner r)

--
-- Pairing two runners in the same signature of operations.
--
pairRunners :: Runner iface iface'' c -> Runner iface' iface'' c'
            -> Runner (IfaceUnion iface iface') iface'' (c,c')
pairRunners Empty r' = extendRunner r'
pairRunners (CoOps coops r) r' =
  CoOps (\ o -> KC (\ (c,c') ->
                  do (x,c'') <- let (KC k) = coops o in k c
                     return (x,(c'',c'))))
        (pairRunners r r')

--
-- Runner that forwards all co-operations to its runtime.
--
fwdRunner :: Member e iface => Runner '[e] iface c
fwdRunner = CoOps genPerformK Empty

--
-- Focussing on a particular effect in a larger signature of operations.
--
focus :: Member e iface => User '[e] a -> User iface a
focus m =
  run fwdRunner (return ()) m (\ x () -> return x)
