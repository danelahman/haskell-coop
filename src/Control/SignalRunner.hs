{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Control.SignalRunner
Description : Effectful runners (in their general form, with support for both exceptions and signals)
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides an implementation of effectful runners of algebraic effects (in their general form,
with support for both exceptions and signals) to run user code (modelled using the `User` monad)
with co-operations implemented as kernel code (modelled using the `Kernel` monad). This module is
based on ongoing research of [Danel Ahman](https://danel.ahman.ee) and [Andrej Bauer](http://www.andrej.com).

Until a proper publication about this research has appeared, you might want to check the
talk [Interacting with external resources using runners (aka comodels)](https://danel.ahman.ee/talks/chocola19.pdf)
for an overview of effectful runners and how we transform them into a programming language construct.
For general background reading on algebraic effects and handlers, we recommend the lecture
notes [What is algebraic about algebraic effects and handlers?](https://arxiv.org/abs/1807.05923). Section 4
of these notes discusses ordinary runners of algebraic effects (also known in the literature as comodels of algebraic effects).

The `User` and `Kernel` monad use internally the [freer-simple](http://hackage.haskell.org/package/freer-simple)
implementation of a free monad on a signature of effects, namely, the `Eff` monad.
-}
module Control.SignalRunner (
  User, Kernel, embedU, embedK, focus, performU, performK,
  raiseU, raiseK, kill, getEnv, setEnv, tryWithU, tryWithK,
  kernel, user, Runner, mkRunner, emptyRunner, SigUnion,
  unionRunners, embedRunner, extendRunner, pairRunners,
  fwdRunner, run, pureTopLevel, topLevel, ioTopLevel,
  Zero, impossible, Member
  ) where

import Control.Monad.Freer.Internal hiding (run)
import Control.Monad.Except
import Control.Monad.State

-- | The monad that we use to model user computations that can perform algebraic
-- operations given by effects in the signature @sig@ (using `performU`), raise
-- exceptions of type @e@, and return values of type @a@. The algebraic operation
-- calls will be implemented by some enveloping runner of type `Runner`.
--
-- The signature @sig@ has type @[* -> *]@, in other words, it is a list of effects.
-- Exactly as in the [freer-simple](http://hackage.haskell.org/package/freer-simple)
-- package, each such effect is meant to be given by a GADT whose constructors
-- denote the "algebraic operations" associated with the given effect.
--
-- Compared to the talk [Interacting with external resources using runners (aka comodels)](https://danel.ahman.ee/talks/chocola19.pdf),
-- the our library currently does not support uniformly attaching an exception type to
-- each of effects in the signature @sig@. Instead, the programmer is currently expected
-- to model effects that can raise exceptions by typing relevant algebraic operations
-- appropriately, and pass the exception as an `Either`-typed return value.
--
-- For instance, the effect of performing file IO could be described using
-- the @FileIO@ effect, given by
--
-- > data FileIO :: * -> * where
-- >   OpenFile  :: FilePath -> IOMode -> FileIO (Either Handle E)
-- >   CloseFile :: Handle -> FileIO (Either () E)
-- >   ReadFile  :: Handle -> FileIO (Either String E)
-- >   WriteFile :: Handle -> String -> FileIO (Either () E)
--
-- where @E@ is a type of IO exceptions. For example, one plausible definition of @E@ could be
--
-- > data E where
-- >   FileNotFound :: FilePath -> E
-- >   InvalidFilehandle :: Handle -> E
-- >   QuotaExceeded :: E
-- >   IncorrectIOMode :: IOMode -> E
-- >   FileHandleAlreadyClosed :: Handle -> E
-- >   IOException :: E
--
-- As such, performing any of the file IO operation in one's code could either result
-- in returning a successful return value in the left branch of `Either`, or raising
-- an exception in @E@ by returning a value in the right branch of `Either`.
--
-- It is worthwhile to point out that some of the exceptions also carry data, which can be
-- useful for debugging reasons, or for finalising resources in the `run` operation below.
-- We also note that one could be flexible about which operations of an effect can raise
-- exceptions and which cannot. After all, exceptions are just part of the typing of
-- individual operations.
--
-- A user computation performing no other effects than file IO, potentially raising exceptions
--  @e@, and returning values of type @a@ would then have type @User '[FileIO] e a@.
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

-- | The monad that we use to model kernel computations. Analogously to user
-- computations, kernel computations can perform algebraic operations given
-- effects in the signature @sig@ (using `performK`), raise exceptions @e@,
-- and return values of type @a@. But differently from user computations,
-- kernel computations additionally have access to runtime state of type @c@,
-- which they can read using `getEnv` and write with `setEnv`, and they can
-- also send (kill) signals (using `kill`).
--
-- The primary use of kernel computations is to implement co-operations of
-- runners, which in turn are then used to run user code using `run`.
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

-- | Embedding a user computation in a larger signature.
embedU :: User sig e a -> User (eff ': sig) e a
embedU (U m) = U (raise m)

-- | Embedding a kernel computation in a larger signature.
embedK :: Kernel sig e s c a -> Kernel (eff ': sig) e s c a
embedK (K k) = K (\ c -> raise (k c))

-- | Focussing on a particular effect in a larger signature.
focus :: Member eff sig => User '[eff] e a -> User sig e a
focus m =
  run
    fwdRunner
    (return ())
    m
    (\ x () -> return x)
    (\ e () -> raiseU e)
    impossible -- forces the set of kill signals of `fwdRunner` to be `Zero`

-- | Performing an algebraic operation of the effect @eff@ in user code.
--
-- For example, to perform a file read in user code one writes
--
-- > performU (ReadFile fileHandle) :: User '[FileIO] Zero (Either String E)
--
-- Observe that the exception index in the type of `performU` is `Zero`,
-- i.e., the empty type. This is so because, as noted earlier, the library
-- does not attach exceptions to effects in the signature but requires the
-- programmer to model them as return values of individual operations.
-- However, we have found that in example programs this is not too troublesome,
-- because in the end one usually exposes more human-readably wrapped
-- generic effects to the programmer. Part of that wrapping is then pattern
-- matching on the exception returned as a `Either`-typed value, and
-- raising it as an exception proper in the `User` monad (with `raiseU`).
performU :: eff a -> User '[eff] Zero a
performU op = U (do x <- send op; return (Left x))

-- | Performing an algebraic operation of the effect @eff@ in user code.
genPerformU :: Member eff sig => eff a -> User sig Zero a
genPerformU op = U (do x <- send op; return (Left x))

-- | Performing an algebraic operation of the effect @eff@ in kernel code.
performK :: eff a -> Kernel '[eff] Zero s c a
performK op = K (\ c -> (do x <- send op; return (Left (Left x,c))))

-- | Performing an algebraic operation of the effect @eff@ in kernel code.
genPerformK :: Member eff sig => eff a -> Kernel sig Zero s c a
genPerformK op = K (\ c -> (do x <- send op; return (Left (Left x,c))))

-- | Raising an exception of type @e@ in user code.
--
-- For instance, when working with file IO, @e@ could be the type
--
-- > data E where
-- >   FileNotFound :: FilePath -> E
-- >   InvalidFilehandle :: Handle -> E
-- >   QuotaExceeded :: E
-- >   IncorrectIOMode :: IOMode -> E
-- >   FileHandleAlreadyClosed :: Handle -> E
-- >   IOException :: E
--
-- that we discussed in the description of the `User` monad.
raiseU :: e -> User sig e a
raiseU e = U (return (Right e))

-- | Raising an exception of type @e@ in user code.
raiseK :: e -> Kernel sig e s c a
raiseK e = K (\ c -> return (Left (Right e,c)))

-- | Sending a (kill) signal of type @s@ in kernel code.
--
-- For instance, when working with file IO, @s@ could be the type
--
-- > data S where
-- >   DiscDisconnected :: S
-- >   IOError :: S
--
-- The role of signals is to indicate unavoidable, unrecoverable circumstances.
-- If a signal is sent in a co-operation when running user code with `run`,
-- the rest of the user code is killed off and control jumps to the finaliser
-- for signals. This is in contrast with exceptions raised by co-operations,
-- from which user code being run with `run` can recover from.
kill :: s -> Kernel sig e s c a
kill s = K (\ c -> return (Right s))

-- | Reading runtime state of type @c@ in kernel code.
getEnv :: Kernel sig e s c c
getEnv = K (\ c -> return (Left (Left c,c)))

-- | Writing runtime state of type @c@ in kernel code.
setEnv :: c -> Kernel sig e s c ()
setEnv c' = K (\ c -> return (Left (Left (),c')))

-- | Exception handler for user code, based on Benton and Kennedy's
-- [exceptional syntax](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/exceptional-syntax/58206FB399EDC9F197A0D53BC46E4667).
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

-- | Exception handler for kernel code, based on Benton and Kennedy's
-- [exceptional syntax](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/exceptional-syntax/58206FB399EDC9F197A0D53BC46E4667).
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

-- | Context switch to execute a kernel computation in user mode.
kernel :: Kernel sig e s c a
      -> c
      -> (a -> c -> User sig e' b)
      -> (e -> c -> User sig e' b)
      -> (s -> User sig e' b)
      -> User sig e' b
kernel (K k) c f g h =
  U (do xs <- k c;
        either
          (\ (xe,c') ->
            either
              (\ x -> let (U m) = f x c' in m)
              (\ e -> let (U m) = g e c' in m)
              xe)
          (\ s -> let (U m) = h s in m)
          xs)

-- | Context switch to execute a user computation in kernel mode.
user :: User sig e a
      -> (a -> Kernel sig e' s c b)
      -> (e -> Kernel sig e' s c b)
      -> Kernel sig e' s c b
user (U m) f g =
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

runAux :: Runner sig sig' s c
       -> c
       -> User sig e a
       -> (a -> c -> User sig' e' b)
       -> (e -> c -> User sig' e' b)
       -> (s -> User sig' e' b)
       -> User sig' e' b
runAux r c (U (Val (Left x))) f g h = f x c
runAux r c (U (Val (Right e))) f g h = g e c
runAux r c (U (E op q)) f g h =
  kernel
    (runOp r op)
    c
    (\ x c' -> runAux r c' (U (qApp q x)) f g h)
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
  do c <- i; runAux r c m f g h

--
-- Running user computation in a top-level pure containers.
--
pureTopLevel :: User '[] Zero a -> a
pureTopLevel (U (Val (Left x))) = x
pureTopLevel _ = error "this should not have happened"

--
-- Running a user computation in a top-level container (monad).
--
--
topLevel :: Monad m => User '[m] Zero a -> m a
topLevel (U m) = runM (fmap (either id impossible) m)

--
-- Short-hand for running user computation ina top-level IO container.
--
ioTopLevel :: User '[IO] Zero a -> IO a
ioTopLevel = topLevel
        
--
-- Empty type and its eliminator.
--
data Zero

impossible :: Zero -> a
impossible x = case x of {}
