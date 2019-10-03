{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

--
-- Effectful runners (without support for exceptions and kill signals).
--

module Control.Monad.Runner (
  User, Kernel,
  performU, performK,
  getEnv, setEnv,
  Runner, mkRunner, emptyRunner, unionRunners, pairRunners,
  run, execU, execK,
  topLevel, pureTopLevel, ioTopLevel,
  embedU, embedK, embedRunner, extendRunner, fwdRunner, focus,
  Member
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
-- the Freer monad, and analogously `sig` is a signature of
-- effects, given by a list of type-functions `* -> *`.
--
-- The type `User sig a` captures the typing judgement
--
--   Gamma  |-^sig  M  :  a
--
newtype User sig a =
  UC (Eff sig a) deriving (Functor,Applicative,Monad)

--
-- Kernel monad modelling kernel computations that can trigger
-- effects in `sig` and access runtime state of type `c`.
-- We use kernel computations to implement co-operations.
--
-- The type `Kernel sig c a` captures the typing judgement
--
--   Gamma  |-^sig  K  :  a @ c
--
newtype Kernel sig c a =
  KC (c -> Eff sig (a,c)) deriving (Functor)

instance Applicative (Kernel sig c) where
  pure v = KC (\ c -> return (v,c))
  (KC f) <*> (KC k) =
    KC (\ c -> do (g,c') <- f c;
                  (x,c'') <- k c';
                  return (g x,c''))

instance Monad (Kernel sig c) where
  return x = KC (\ c -> return (x,c))
  (KC k) >>= f =
    KC (\ c -> do (x,c') <- k c;
                  let (KC l) = f x in l c')

--
-- Performing user operations while focussed on a single effect.
--
-- If working with a bigger signature, first focus on a single
-- effect before calling perform. This is a small price to
-- pay to get more intricate GADT-based signatures (such as
-- various kinds of state) to work conveniently. In most cases
-- focussing is limited to defining derived generic effects.
--
performU :: eff r -> User '[eff] r
performU op = UC (send op)

--
-- Generic user perform function used internally in this module.
--
genPerformU :: Member eff sig => eff r -> User sig r
genPerformU op = UC (send op)

--
-- Performing kernel operations while focussed on a single effect.
--
performK :: eff r -> Kernel '[eff] c r
performK op = KC (\ c -> do x <- send op; return (x,c))

--
-- Generic kernel perform function used internally in this module.
--
genPerformK :: Member eff sig => eff r -> Kernel sig c r
genPerformK op = KC (\ c -> do x <- send op; return (x,c))

--
-- State-access operations for the kernel monad.
--
getEnv :: Kernel sig c c
getEnv = KC (\ c -> return (c,c))

setEnv :: c -> Kernel sig c ()
setEnv c' = KC (\ c -> return ((),c'))

--
-- Executing a kernel computation inside user computations.
--
execU :: Kernel sig c a -> c -> (a -> c -> User sig b) -> User sig b
execU (KC k) c f =
  UC (do (x,c') <- k c; let (UC m) = f x c' in m)

--
-- Executing a user computation inside kernel computations.
--
execK :: User sig a -> (a -> Kernel sig c b) -> Kernel sig c b
execK (UC m) f =
  KC (\ c -> do x <- m; let (KC k) = f x in k c)

--
-- A runner is simply a list of co-operations, where the first
-- argument of CoOps requires a co-operation for each operation
-- in the given effect `e :: * -> *`, e.g., for `FRead` and `FWrite`.
--
data Runner sig sig' c where
  Empty :: Runner '[] sig' c
  CoOps :: (forall b . eff b -> Kernel sig' c b)
        -> Runner sig sig' c-> Runner (eff ': sig) sig' c

mkRunner :: (forall b . eff b -> Kernel sig c b) -> Runner '[eff] sig c
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
runOp :: Runner sig sig' c -> Union sig b -> Kernel sig' c b
runOp Empty _ =
  error "this should not have happened"
runOp (CoOps coop coops) u =
  case decomp u of
    Right o -> coop o
    Left u -> runOp coops u

runU :: Runner sig sig' c
     -> c
     -> User sig a
     -> (a -> c -> User sig' b)
     -> User sig' b
runU r c (UC (Val x)) mf = mf x c
runU r c (UC (E u q)) mf =
  execU (runOp r u) c
        (\ x c' -> runU r c' (UC (qApp q x)) mf)

run :: Runner sig sig' c
    -> User sig' c
    -> User sig a
    -> (a -> c -> User sig' b)
    -> User sig' b
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
emptyRunner :: Runner '[] sig' c
emptyRunner = Empty

type family SigUnion (sig :: [* -> *]) (sig' :: [* -> *]) :: [* -> *] where 
  SigUnion '[] sig' = sig'
  SigUnion (eff ': sig) sig' = eff ': (SigUnion sig sig')

unionRunners :: Runner sig sig'' c -> Runner sig' sig'' c
             -> Runner (SigUnion sig sig') sig'' c
unionRunners Empty r' = r'
unionRunners (CoOps coops r) r' =
  CoOps coops (unionRunners r r')

--
-- Embedding a user computations in a larger signature of operations.
--
embedU :: User sig a -> User (eff ': sig) a
embedU (UC m) = UC (raise m)

--
-- Embedding a kernel computations in a larger signature of operations.
--
embedK :: Kernel sig c a -> Kernel (eff ': sig) c a
embedK (KC k) = KC (\ c -> do (x,c') <- raise (k c);
                              return (x,c'))

--
-- Embedding a runner in a larger signature of operations.
--
embedRunner :: Runner sig sig' c -> Runner sig (eff ': sig') c
embedRunner Empty = Empty
embedRunner (CoOps coops r) =
  CoOps (\ op -> embedK (coops op)) (embedRunner r)

--
-- Extending the carrier of a runner with some type/set.
--
extendRunner :: Runner sig sig' c' -> Runner sig sig' (c,c')
extendRunner Empty = Empty
extendRunner (CoOps coops r) =
  CoOps (\ op -> KC (\ (c,c') ->
                   do (x,c'') <- let (KC k) = coops op in k c';
                      return (x,(c,c''))))
        (extendRunner r)

--
-- Pairing two runners in the same signature of operations.
--
pairRunners :: Runner sig sig'' c -> Runner sig' sig'' c'
            -> Runner (SigUnion sig sig') sig'' (c,c')
pairRunners Empty r' = extendRunner r'
pairRunners (CoOps coops r) r' =
  CoOps (\ op -> KC (\ (c,c') ->
                   do (x,c'') <- let (KC k) = coops op in k c
                      return (x,(c'',c'))))
        (pairRunners r r')

--
-- Runner that forwards all co-operations to its runtime.
--
fwdRunner :: Member eff sig => Runner '[eff] sig c
fwdRunner = CoOps genPerformK Empty

--
-- Focussing on a particular effect in a larger signature of operations.
--
focus :: Member eff sig => User '[eff] a -> User sig a
focus m =
  run fwdRunner (return ()) m (\ x () -> return x)
