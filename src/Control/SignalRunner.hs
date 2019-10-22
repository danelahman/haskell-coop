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
-- exceptions of type @e@ (using `raiseU`), and return values of type @a@.
-- The algebraic operation calls will be implemented by some enveloping runner of type `Runner`.
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

-- | The monad that we use to model kernel computations. Analogously to user
-- computations, kernel computations can perform algebraic operations given
-- effects in the signature @sig@ (using `performK`), raise exceptions of type @e@
-- (using `raiseK`), and return values of type @a@. But differently from user
-- computations, kernel computations additionally have access to runtime state of type @c@,
-- which they can read using `getEnv` and write with `setEnv`, and they can
-- also send (kill) signals of type @s@ (using `kill`).
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
-- > performU (ReadFile fileHandle) :: User '[FileIO] e (Either String E)
--
-- As discussed in the description of `User`, the library currently
-- does not support attaching exceptions to effects in a signature.
-- Instead the programmer is expected to model any exceptions returned
-- by an operation as an exceptional return values, as above. In example
-- programs we have found that this style is not too troublesome,
-- because in the end one usually exposes more human-readably wrapped
-- generic effects to the programmer. Part of that wrapping is then 
-- pattern matching on the exception returned as a `Either`-typed value,
-- and raising it as an exception proper in the `User` monad (using
-- `raiseU`), e.g., see the discussion in the description of `run`.
--
-- Also observe that the exception index @e@ is left polymorphic above---it
-- gets instantiated by the contex in which one places this operation call.
--
performU :: eff a -> User '[eff] e a
performU op = U (do x <- send op; return (Left x))

-- | Performing an algebraic operation of the effect @eff@ in user code.
genPerformU :: Member eff sig => eff a -> User sig e a
genPerformU op = U (do x <- send op; return (Left x))

-- | Performing an algebraic operation of the effect @eff@ in kernel code.
performK :: eff a -> Kernel '[eff] e s c a
performK op = K (\ c -> (do x <- send op; return (Left (Left x,c))))

-- | Performing an algebraic operation of the effect @eff@ in kernel code.
genPerformK :: Member eff sig => eff a -> Kernel sig e s c a
genPerformK op = K (\ c -> (do x <- send op; return (Left (Left x,c))))

-- | Raising an exception of type @e@ in user code.
--
-- Raised user exceptions can be caught with `tryWithU`, and with
-- the finalisers of `run` and `user`.
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
--
-- Raised kernel exceptions can be caught with `tryWithK`,
-- and with the finalisers of `kernel`.
raiseK :: e -> Kernel sig e s c a
raiseK e = K (\ c -> return (Left (Right e,c)))

-- | Sending a (kill) signal of type @s@ in kernel code.
--
-- Signals can be caught with the finalisers of `run` and `kernel`.
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
--
-- The 1st argument of type @Kernel sig e s c a@ is the kernel computation to be executed.
--
-- The 2nd argument of type @c@ is the initial value for runtime state.
--
-- The 3rd argument of type @a -> c -> User sig e' b@ is a finaliser for return values.
--
-- The 4th argument of type @e -> c -> User sig e' b@ is a finaliser for exceptions.
--
-- The 5th argument of type @s -> User sig e' b@ is a finaliser for signals.
--
-- The 3rd, 4th, and 5th argument perform the context switch back to user mode.
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
--
-- The 1st argument of type @User sig e a@ is the user computation to be executed.
--
-- The 2nd argument of type @a -> Kernel sig e' s c b@ is a finaliser for return values.
--
-- The 3rd argument of type @e -> Kernel sig e' s c b@ is a finaliser for exceptions.
--
-- The 2nd and 3rd argument perform the context switch back to kernel mode.
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

-- | Type of effectful runners that implement co-operations for the effects in
-- signature @sig@, where each of the co-operations is a kernel computation that
-- can perform algebraic operations given by (external) effects in the signature
-- @sig'@, send (kill) signals @s@, and access runtime state of type @c@. The
-- exception index in the type of each co-operation is `Zero`, i.e., the empty
-- type, because currently we do not attach exceptions to effects in signatures.
-- As a result, any exceptions have to be returned as `Either`-typed values.
-- For example, see the discussion in the descriptions of `User` and `performU`.
--
-- Given an effect @eff :: * -> *@, the corresponding co-operations are given by a
-- function of type
--
-- > forall b . eff b -> Kernel sig' c b
--
-- in other words, by a mapping of every algebraic operation of the effect @eff@
-- (i.e., each of its constructors) to a corresponding kernel computation.
data Runner sig sig' s c where
  Empty :: Runner '[] sig' s c
  CoOps :: (forall b . eff b -> Kernel sig' Zero s c b)
        -> Runner sig sig' s c-> Runner (eff ': sig) sig' s c

-- | Make a runner for a single effect @eff@ by providing co-operations implementing
-- each of its algebraic operations.
--
-- For instance, a runner whose runtime state carries a file handle and that
-- implements a co-operation for write-only file access could be given by
--
-- > mkRunner (\ (Write s) -> do fh <- getEnv; performK (WriteFile fh s)) :: Runner '[WriteIO] '[FileIO] S (Either Handle E)
--
-- where we assume that the effect @eff@ is given by
--
-- > data WriteIO :: * -> * where
-- >   Write :: String -> FileIO (Either () E)
--
-- where @E@ is some type of IO exceptions (e.g., see the description of `raiseU`), and
-- where @S@ is some type of IO signals (e.g., containing a @DiskDisconnected@ signal
-- to model the possibility of a remote disk getting disconnected unexpectedly).
mkRunner :: (forall b . eff b -> Kernel sig Zero s c b)
         -> Runner '[eff] sig s c
mkRunner coops = CoOps coops Empty

-- | Runner for the empty signature.
emptyRunner :: Runner '[] sig' s c
emptyRunner = Empty

-- | The (disjoint) union of two signatures.
type family SigUnion (sig :: [* -> *]) (sig' :: [* -> *]) :: [* -> *] where 
  SigUnion '[] sig' = sig'
  SigUnion (eff ': sig) sig' = eff ': (SigUnion sig sig')

-- | Taking the union of (the co-operations of) two runners with the same
-- external signature @sig''@, (kill) signals @s@, and runtime state @c@.
-- The resulting runner implements co-operations for the union of the given signatures.
--
-- The intended use of `unionRunners` is to build a runner for a composite
-- signature from runners for individual effects given by `mkRunner`.
unionRunners :: Runner sig sig'' s c -> Runner sig' sig'' s c
             -> Runner (SigUnion sig sig') sig'' s c
unionRunners Empty r' = r'
unionRunners (CoOps coops r) r' =
  CoOps coops (unionRunners r r')

-- | Embedding a runner in a larger external signature.
embedRunner :: Runner sig sig' s c -> Runner sig (eff ': sig') s c
embedRunner Empty = Empty
embedRunner (CoOps coops r) =
  CoOps (\ op -> embedK (coops op)) (embedRunner r)

-- | Extending the runtime state with an additional component,
-- which the co-operations of the resulting runner keep unchanged.
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

-- | Pairing two runners with the same external signature @sig''@
-- but with possibly different runtime state types @c@ and @c'@. The
-- resulting runner implements co-operations for the disjoint union 
-- of the given signatures, by executing first runner's co-operations 
-- on the first part of the composite runtime state, and the second
-- runner's co-operations on the second part of the composite state.
--
-- In other words, the resulting runner runs the given runners
-- side-by-side, in a kind of horizontal composition.
--
-- The intended use of `pairRunners` is to construct n-ary combinations
-- of individual runners, e.g., by combining some number of file IO
-- runners with some number of runners implementing ML-style state.
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

-- | Runner that forwards all of its co-operations to some enveloping runner.
fwdRunner :: Member eff sig => Runner '[eff] sig s c
fwdRunner = CoOps genPerformK Empty

-- | Running a single algebraic operation as a kernel computation using the given runner.
runOp :: Runner sig sig' s c -> Union sig b -> Kernel sig' Zero s c b
runOp Empty _ =
  error "this should not have happened"
runOp (CoOps coop coops) u =
  case decomp u of
    Right o -> coop o
    Left u -> runOp coops u

-- | Auxiliary operation for running user computations using a given runner in which
-- the initial runtime state is initialised by a value rather than an effectful user
-- computation as in `run`.
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

-- | A programming construct to run user code using a runner with guaranteed finalisation.
--
-- The 1st argument (of type @Runner sig sig' c@) is the given runner.
--
-- The 2nd argument (of type @User sig' e' c@) is a user computation that produces an initial
-- value for the runtime state that the runner operates on.
--
-- The 3rd argument (of type @User sig e a@) is the user computation that we are running
-- using the given runner. Observe that this user computation can only perform the
-- algebraic operations for the effects in the signature @sig@ implemented by the runner.
-- It cannot directly perform algebraic operations from the (external) signature @sig'@.
-- It can only do so if the runner explicitly forwards the needed algebraic operations.
--
-- The 4th argument (of type @a -> c -> User sig' e' b@) is a user computation that
-- finalises for return values. Notice that in addition to having access to return values,
-- it can also access the final value of the runtime state, so as to perform cleanup.
--
-- The 5th argument (of type @e -> c -> User sig' e' b@) is a user computation that
-- finalises for exceptions. As with the finaliser for return values, this computation
-- also has access to the final value of the rutime state, so as to perform cleanup.
--
-- The 6th argument (of type @s -> User sig' e' b@) is a user computation that
-- finalises for (kill) signals. In contrast witht the finalisers for return values and
-- exceptions, this computation does not have access to the final runtime state.
--
-- For instance, we can run a simple user computation using the write-only file access
-- runner defined in the description of `mkRunner` as follows
--
-- > run
-- >   (mkRunner (\ (Write s) -> do fh <- getEnv; performK (WriteFile fh s)))
-- >   (open "hello.txt" WriteMode)
-- >   (write "Hello, world."; write "Hello, again.")
-- >   (\ x fh -> close fh; return x)
-- >   (\ e fh -> close fh; raiseU e)
-- >   (\ s -> return ())
-- >   :: User '[FileIO] E () 
--
-- where @open@, @write@, and @close@ are the human-readably wrapped generic effect
-- for performing @OpenFile@, @Write@, and @CloseFile@ operations, defined as follows
--
-- > open fn m =
-- >   do xe <- performU (OpenFile fn m);
-- >      either (\ x -> return x) (\ e -> raiseU e) xe
-- > :: User '[FileIO] E Handle
--
-- > write fn m =
-- >   do xe <- performU (Write s);
-- >      either (\ x -> return x) (\ e -> raiseU e) xe
-- > :: User '[WriteIO] E ()
--
-- > close fh =
-- >   do xe <- performU (CloseFile fh);
-- >      either (\ x -> return x) (\ e -> raiseU e) xe
-- > :: User '[FileIO] E ()
--
-- Observe that these generic effects pattern-matches on the `Either`-typed value returned 
-- by `performU`, and raise any exception values as exceptions proper with `raiseU`.
--
-- Above we initialise the runtime state for the write-only file access runner
-- with a file handle pointing to @"hello.txt"@, by performing a file open
-- operation that would be implemented by some enveloping runner. The user
-- computation that we run with said runner simply performs two calls to
-- the @Write@ operation. Finally, the finaliser for return values closes the
-- file handle and passes on the return value unchanged; the finaliser for
-- exceptions also closes the file handle, but re-raises the exception; and
-- the finaliser for signals simply returns the unit value, the intuition
-- being that once a signal is sent, there are no resources to finalise.
--
-- Observe how this write-only file access runner hides the file handle from
-- the user code being run (the latter can only perform the @Write@ operation
-- that takes only a string as an argument and not the file handle itself).
-- Furthermore, the semantics of the `run` operation ensures that one of the 
-- finalisers is always called exactly once, ensuring a correct cleanup of the
-- file handle resource, as desired. Though this last part has to be taken with
-- a pinch of salt. If a signal is raised in a co-operation of some outer, enveloping
-- runner that is used to run the above code-snippet, then control jumps to the
-- finalisation block of this outer `run` operation, and the inner code gets killed.
--
-- In the context of the
-- talk [Interacting with external resources using runners (aka comodels)](https://danel.ahman.ee/talks/chocola19.pdf),
-- the `run` operation corresponds to the following programming construct
--
-- > using R @ M_init
-- > run M
-- > finally {
-- >   return x @ c -> M_return,
-- >   (raise e @ c -> M_e)_{e \in E},
-- >   (kill s -> M_s)_{s \in S}}
run :: Runner sig sig' s c
    -> User sig' e' c
    -> User sig e a
    -> (a -> c -> User sig' e' b)
    -> (e -> c -> User sig' e' b)
    -> (s -> User sig' e' b)
    -> User sig' e' b
run r i m f g h =
  do c <- i; runAux r c m f g h

-- | A top-level for running user computations for the empty signature as pure, effect-free values.
pureTopLevel :: User '[] Zero a -> a
pureTopLevel (U (Val (Left x))) = x
pureTopLevel _ = error "this should not have happened"

-- | A top-level for running user computations as Haskell's monadic computations.
topLevel :: Monad m => User '[m] Zero a -> m a
topLevel (U m) = runM (fmap (either id impossible) m)

-- | Syntactic sugar for top-level running of user computations in the IO monad, defined using `topLevel`.
ioTopLevel :: User '[IO] Zero a -> IO a
ioTopLevel = topLevel
        
-- | The empty type that does not have any constructors.
data Zero

-- | The elimination form for the empty type.
impossible :: Zero -> a
impossible x = case x of {}
