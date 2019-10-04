{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

--
-- Effectful runners (with support for exceptions and kill signals).
--

module Control.Monad.SignalRunner (
--  User, Kernel,
--  performU, performK,
--  getEnv, setEnv,
--  Runner, mkRunner, emptyRunner, unionRunners, pairRunners,
--  run, execU, execK,
--  topLevel, pureTopLevel, ioTopLevel,
--  embedU, embedK, embedRunner, extendRunner, fwdRunner, focus,
--  Member
  ) where

--
-- Tested with
--   http://hackage.haskell.org/package/freer-simple [v1.2.1.0]
-- variant of the Freer monad.
--
import Control.Monad.Freer.Internal hiding (run)

import Control.Monad.Except
import Control.Monad.State

{-
--
-- Exceptional signatures and a their conversion into
-- ordinary Freer monad signatures using `Either`.
--
type ExcSig = [((* -> *),*)]

--newtype EffExcToEff (eff :: * -> *) (exc :: *) (a :: *) =
--  ExcEff (eff (Either exc a))

type EffExcToEff (eff :: * -> *) (exc :: *) (a :: *) =
  eff (Either exc a)

type family ExcSigToSig (sig :: ExcSig) :: [* -> *] where
  ExcSigToSig '[] = '[]
  ExcSigToSig ('(eff,exc) ': sig) =
    (EffExcToEff eff exc) ': ExcSigToSig sig
-}

--
-- User monad modelling user computations waiting to be run
-- by some enveloping runner. We define this monad on top of the
-- Freer monad, with `sig` a signature of effects of type `[* -> *]`.
--
-- The type `User sig a` captures the typing judgement
--
--   Gamma  |-^sig  M  :  a ! e
--
--
newtype User sig e a =
  U (Eff sig (Either a e))

fmapE :: (a -> b) -> Either a e -> Either b e
fmapE f (Left x) = Left (f x)
fmapE f (Right e) = Right e

instance Functor (User sig e) where
  fmap f (U m) = U (fmap (fmapE f) m)

appE :: Either (a -> b) e -> Either a e -> Either b e
appE (Left f) (Left x) = Left (f x)
appE (Left f) (Right e) = Right e
appE (Right e) _ = Right e

bindU :: User sig e a -> (a -> User sig e b) -> User sig e b
bindU m f = tryWithU m f (\ e -> raiseU e)

instance Applicative (User sig e) where
  pure x = U (pure (Left x))
  f <*> m = bindU f (\ g -> bindU m (\ x -> pure (g x)))

instance Monad (User sig e) where
  return x = pure x
  m >>= f = bindU m f

--
-- Kernel monad modelling kernel computations that can trigger
-- effects in `sig` and access runtime state of type `c`,
-- and raise exceptions in `e` and kill signals in `s`.
-- We use kernel computations to implement co-operations.
--
-- The type `Kernel sig e s c a` captures the typing judgement
--
--   Gamma  |-^sig  K  :  a ! e !! s @ c
--
newtype Kernel sig e s c a =
  K (c -> (Eff sig (Either (Either a e,c) s)))

instance Functor (Kernel sig e s c) where
  fmap f (K k) =
    K (\ c -> fmap (fmapE (\ (xe,c) -> (fmapE f xe,c))) (k c))

bindK :: Kernel sig e s c a
      -> (a -> Kernel sig e s c b)
      -> Kernel sig e s c b
bindK k f = tryWithK k f (\ e -> raiseK e)

instance Applicative (Kernel sig e s c) where
  pure x = K (\ c -> pure (Left (Left x,c)))
  f <*> k = bindK f (\ g -> bindK k (\ x -> pure (g x)))

instance Monad (Kernel sig e s c) where
  return x = pure x
  k >>= f = bindK k f

--
-- Performing user operations while focussed on a single effect.
--
-- If working with a bigger signature, first focus on a single
-- effect before calling perform. This is a small price to
-- pay to get more intricate GADT-based signatures (such as
-- various kinds of state) to work conveniently. In most cases
-- focussing is limited to defining derived generic effects.
--
performU :: eff a -> User '[eff] () a
performU op = U (do x <- send op; return (Left x))

--
-- Generic user perform function used internally in this module.
--
genPerformU :: Member eff sig => eff a -> User sig () a
genPerformU op = U (do x <- send op; return (Left x))

--
-- Raising an exception in user computations.
--
raiseU :: e -> User sig e a
raiseU e = U (return (Right e))
--
-- Performing kernel operations while focussed on a single effect.
--
performK :: eff a -> Kernel '[eff] () s c a
performK op = K (\ c -> (do x <- send op; return (Left (Left x,c))))

--
-- Generic kernel perform function used internally in this module.
--
genPerformK :: Member eff sig => eff a -> Kernel sig () s c a
genPerformK op = K (\ c -> (do x <- send op; return (Left (Left x,c))))

--
-- Raising an exception in kernel computations.
--
raiseK :: e -> Kernel sig e s c a
raiseK e = K (\ c -> return (Left (Right e,c)))

--
-- Raising a kill signal in kernel computations.
--
kill :: s -> Kernel sig e s c a
kill s = K (\ c -> return (Right s))

--
-- State-access operations for the kernel monad.
--
getEnv :: Kernel sig e s c c
getEnv = K (\ c -> return (Left (Left c,c)))

setEnv :: c -> Kernel sig e s c ()
setEnv c' = K (\ c -> return (Left (Left (),c')))

--
-- Exceptional syntax for user computations.
--
tryWithU :: User sig e a
         -> (a -> User sig e' b)
         -> (e -> User sig e' b)
         -> User sig e' b
tryWithU (U m) f g =
  U (do ex <- m;
        either
          (\ x -> let (U m') = f x in m')
          (\ e -> let (U m') = g e in m')
          ex)

--
-- Exceptional syntax for kernel computations.
--
tryWithK :: Kernel sig e s c a
         -> (a -> Kernel sig e' s c b)
         -> (e -> Kernel sig e' s c b)
         -> Kernel sig e' s c b
tryWithK (K k) f g =
  K (\ c ->
    do xs <- k c;
       either
         (\ (xe,c') ->
           either
             (\ x -> let (K f') = f x in f' c')
             (\ e -> let (K g') = g e in g' c')
             xe)
         (\ s -> return (Right s))
         xs)

--
-- Executing a kernel computation inside user computations.
--
execU :: Kernel sig e s c a
      -> c
      -> (a -> c -> User sig e' b)
      -> (e -> c -> User sig e' b)
      -> (s -> User sig e' b)
      -> User sig e' b
execU (K k) c f g h =
  U (do xs <- k c;
        either
          (\ (xe,c') ->
            either
              (\ x -> let (U m) = f x c' in m)
              (\ e -> let (U m) = g e c' in m)
              xe)
          (\ s -> let (U m) = h s in m)
          xs)

{-
--
-- Executing a user computation inside kernel computations.
--
execK :: User sig a -> (a -> Kernel sig c b) -> Kernel sig c b
execK (UC m) f =
  KC (\ c -> do x <- m; let (KC k) = f x in k c)

--
-- A runner is simply a list of co-operations, where the first
-- argument of CoOps requires a co-operation for each operation
-- in the given effect `eff :: * -> *`, e.g., for `FRead` and `FWrite`.
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
-}