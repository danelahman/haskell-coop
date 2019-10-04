{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
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
  User, Kernel,
  performU, performK,
  raiseU, raiseK, kill, 
  getEnv, setEnv,
  Runner, mkRunner, emptyRunner, unionRunners, pairRunners,
  tryWithU, tryWithK, 
  run, execU, execK,
  topLevel, pureTopLevel, ioTopLevel,
  embedU, embedK, embedRunner, extendRunner, fwdRunner, focus,
  Zero, impossible,
  Member
  ) where

--
-- Tested with
--   http://hackage.haskell.org/package/freer-simple [v1.2.1.0]
-- variant of the Freer monad.
--
import Control.Monad.Freer.Internal hiding (run)

import Control.Monad.Except
import Control.Monad.State

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
-- Empty type and its eliminator.
--
data Zero

impossible :: Zero -> a
impossible x = case x of {}

--
-- Performing user operations while focussed on a single effect.
--
-- If working with a bigger signature, first focus on a single
-- effect before calling perform. This is a small price to
-- pay to get more intricate GADT-based signatures (such as
-- various kinds of state) to work conveniently. In most cases
-- focussing is limited to defining derived generic effects.
--
-- Note: In the given implementation, operations are expected to
-- encode any exceptions they might raise in their output type
-- using, e.g., `Either`, and explicitly pattern-match at call
-- sites. As such, `performU` (and `performK`) do not raise any
-- exceptions in the User and Kernel monads, cf use of `Zero`.
--
-- In future, we hopefully also have some variant of exceptional
-- signatures working, see the bottom of the page of a sketch.
--
performU :: eff a -> User '[eff] Zero a
performU op = U (do x <- send op; return (Left x))

--
-- Generic user perform function used internally in this module.
--
genPerformU :: Member eff sig => eff a -> User sig Zero a
genPerformU op = U (do x <- send op; return (Left x))

--
-- Raising an exception in user computations.
--
raiseU :: e -> User sig e a
raiseU e = U (return (Right e))
--
-- Performing kernel operations while focussed on a single effect.
--
performK :: eff a -> Kernel '[eff] Zero s c a
performK op = K (\ c -> (do x <- send op; return (Left (Left x,c))))

--
-- Generic kernel perform function used internally in this module.
--
genPerformK :: Member eff sig => eff a -> Kernel sig Zero s c a
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

--
-- Executing a user computation inside kernel computations.
--
execK :: User sig e a
      -> (a -> Kernel sig e' s c b)
      -> (e -> Kernel sig e' s c b)
      -> Kernel sig e' s c b
execK (U m) f g =
  K (\ c -> do xe <- m;
               either
                 (\ x -> let (K k) = f x in k c)
                 (\ e -> let (K k) = g e in k c)
                 xe)

--
-- A runner is simply a list of co-operations, where the first
-- argument of CoOps requires a co-operation for each operation
-- in the given effect `eff :: * -> *`, e.g., for `FRead`.
--
data Runner sig sig' s c where
  Empty :: Runner '[] sig' s c
  CoOps :: (forall b . eff b -> Kernel sig' Zero s c b)
        -> Runner sig sig' s c-> Runner (eff ': sig) sig' s c

mkRunner :: (forall b . eff b -> Kernel sig Zero s c b)
         -> Runner '[eff] sig s c
mkRunner coops = CoOps coops Empty

--
-- The
--
--  using C @ m_init
--  run m
--  finally { return x @ c -> m_val ;
--            raise  e @ c -> m_exc ;
--            kill   s     -> m_sig }
--
-- construct for initialising, running, and finalising a user computation.
--
runOp :: Runner sig sig' s c -> Union sig b -> Kernel sig' Zero s c b
runOp Empty _ =
  error "this should not have happened"
runOp (CoOps coop coops) u =
  case decomp u of
    Right o -> coop o
    Left u -> runOp coops u

runU :: Runner sig sig' s c
     -> c
     -> User sig e a
     -> (a -> c -> User sig' e' b)
     -> (e -> c -> User sig' e' b)
     -> (s -> User sig' e' b)
     -> User sig' e' b
runU r c (U (Val (Left x))) f g h = f x c
runU r c (U (Val (Right e))) f g h = g e c
runU r c (U (E op q)) f g h =
  execU
    (runOp r op)
    c
    (\ x c' -> runU r c' (U (qApp q x)) f g h)
    (\ e c' -> impossible e)
    (\ s -> h s)

run :: Runner sig sig' s c
    -> User sig' e' c
    -> User sig e a
     -> (a -> c -> User sig' e' b)
     -> (e -> c -> User sig' e' b)
     -> (s -> User sig' e' b)
    -> User sig' e' b
run r i m f g h =
  do c <- i; runU r c m f g h

--
-- Running a user computation in a top-level container (monad).
--
--
topLevel :: Monad m => User '[m] Zero a -> m a
topLevel (U m) = runM (fmap (either id impossible) m)

--
-- Running user computation in a top-level pure containers.
--
pureTopLevel :: User '[] Zero a -> a
pureTopLevel (U (Val (Left x))) = x
pureTopLevel _ = error "this should not have happened"

--
-- Short-hand for running user computation ina top-level IO container.
--
ioTopLevel :: User '[IO] Zero a -> IO a
ioTopLevel = topLevel

--
-- Empty runner and union of runners.
--
emptyRunner :: Runner '[] sig' s c
emptyRunner = Empty

type family SigUnion (sig :: [* -> *]) (sig' :: [* -> *]) :: [* -> *] where 
  SigUnion '[] sig' = sig'
  SigUnion (eff ': sig) sig' = eff ': (SigUnion sig sig')

unionRunners :: Runner sig sig'' s c -> Runner sig' sig'' s c
             -> Runner (SigUnion sig sig') sig'' s c
unionRunners Empty r' = r'
unionRunners (CoOps coops r) r' =
  CoOps coops (unionRunners r r')

--
-- Embedding a user computations in a larger signature of operations.
--
embedU :: User sig e a -> User (eff ': sig) e a
embedU (U m) = U (raise m)

--
-- Embedding a kernel computations in a larger signature of operations.
--
embedK :: Kernel sig e s c a -> Kernel (eff ': sig) e s c a
embedK (K k) = K (\ c -> raise (k c))

--
-- Embedding a runner in a larger signature of operations.
--
embedRunner :: Runner sig sig' s c -> Runner sig (eff ': sig') s c
embedRunner Empty = Empty
embedRunner (CoOps coops r) =
  CoOps (\ op -> embedK (coops op)) (embedRunner r)

--
-- Extending the carrier of a runner with some type/set.
--
extendRunner :: Runner sig sig' s c' -> Runner sig sig' s (c,c')
extendRunner Empty = Empty
extendRunner (CoOps coops r) =
  CoOps (\ op -> K (\ (c,c') ->
                     do xs <- let (K k) = coops op in k c';
                        either
                          (\ (xe,c'') -> return (Left (xe,(c,c''))))
                          (\ s -> return (Right s))
                          xs))
        (extendRunner r)

--
-- Pairing two runners in the same signature of operations.
--
pairRunners :: Runner sig sig'' s c -> Runner sig' sig'' s c'
            -> Runner (SigUnion sig sig') sig'' s (c,c')
pairRunners Empty r' = extendRunner r'
pairRunners (CoOps coops r) r' =
  CoOps (\ op -> K (\ (c,c') ->
                     do xs <- let (K k) = coops op in k c
                        either
                          (\ (xe,c'') -> return (Left (xe,(c'',c'))))
                          (\ s -> return (Right s))
                          xs))
        (pairRunners r r')
        
--
-- Runner that forwards all co-operations to its runtime.
--
fwdRunner :: Member eff sig => Runner '[eff] sig s c
fwdRunner = CoOps genPerformK Empty

--
-- Focussing on a particular effect in a larger signature of operations.
--
-- Here the use of `impossible` in fact forces the set of kill signals 
-- of the given runner `fwdRunner` to be `Zero`.
--
focus :: Member eff sig => User '[eff] e a -> User sig e a
focus m =
  run
    fwdRunner
    (return ())
    m
    (\ x () -> return x)
    (\ e () -> raiseU e)
    impossible


{-
--
-- TODO: Make exceptional signatures in this style work.
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
