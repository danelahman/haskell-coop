{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Control.Runner
Description : Effectful runners (in a restricted form, without support for exceptions and signals)
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides an implementation of effectful runners of algebraic effects (in a restricted form,
without support for exceptions and signals) to run user code (modelled using the `User` monad)
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
module Control.Runner (
  User, Kernel, embedU, embedK, focus, performU, performK,
  getEnv, setEnv, kernel, user, Runner, mkRunner, emptyRunner,
  SigUnion, unionRunners, embedRunner, extendRunner, pairRunners,
  fwdRunner, run, pureTopLevel, topLevel, ioTopLevel, Member
  ) where

import Control.Monad.Freer.Internal hiding (run)

-- | The monad that we use to model user computations that can perform algebraic
-- operations given by effects in the signature @sig@ (using `performU`) and return 
-- values of type @a@. The algebraic operation calls will be implemented by some 
-- enveloping runner of type `Runner`.
--
-- The signature @sig@ has type @[* -> *]@, in other words, it is a list of effects.
-- Exactly as in the [freer-simple](http://hackage.haskell.org/package/freer-simple)
-- package, each such effect is meant to be given by a GADT whose constructors
-- denote the "algebraic operations" associated with the given effect.
--
-- For instance, the effect of performing file IO could be described using
-- the @FileIO@ effect, given by
--
-- > data FileIO :: * -> * where
-- >   OpenFile  :: FilePath -> IOMode -> FileIO Handle
-- >   CloseFile :: Handle -> FileIO ()
-- >   ReadFile  :: Handle -> FileIO String
-- >   WriteFile :: Handle -> String -> FileIO ()
--
-- A user computation performing no other effects than file IO and returning
-- values of type @a@ would then have type @User '[FileIO] a@.
newtype User sig a =
  UC (Eff sig a) deriving (Functor,Applicative,Monad)

-- | The monad that we use to model kernel computations. Analogously to user
-- computations, kernel computations can perform algebraic operations given
-- effects in the signature @sig@ (using `performK`) and return values of
-- type @a@. But differently from user computations, kernel computations
-- additionally have access to runtime state of type @c@, which they can
-- read using `getEnv` and write with `setEnv`.
--
-- The primary use of kernel computations is to implement co-operations of
-- runners, which in turn are then used to run user code using `run`.
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

-- | Embedding a user computation in a larger signature.
embedU :: User sig a -> User (eff ': sig) a
embedU (UC m) = UC (raise m)

-- | Embedding a kernel computation in a larger signature.
embedK :: Kernel sig c a -> Kernel (eff ': sig) c a
embedK (KC k) = KC (\ c -> raise (k c))

-- | Focussing on a particular effect in a larger signature.
focus :: Member eff sig => User '[eff] a -> User sig a
focus m =
  run fwdRunner (return ()) m (\ x () -> return x)

-- | Performing an algebraic operation of the effect @eff@ in user code.
--
-- For example, to perform a file read in user code one writes
--
-- > performU (ReadFile fileHandle) :: User '[FileIO] String
performU :: eff a -> User '[eff] a
performU op = UC (send op)

-- | Performing an algebraic operation of the effect @eff@ in user code.
genPerformU :: Member eff sig => eff a -> User sig a
genPerformU op = UC (send op)

-- | Performing an algebraic operation of the effect @eff@ in kernel code.
performK :: eff a -> Kernel '[eff] c a
performK op = KC (\ c -> do x <- send op; return (x,c))

-- | Performing an algebraic operation of the effect @eff@ in kernel code.
genPerformK :: Member eff sig => eff a -> Kernel sig c a
genPerformK op = KC (\ c -> do x <- send op; return (x,c))

-- | Reading runtime state of type @c@ in kernel code.
getEnv :: Kernel sig c c
getEnv = KC (\ c -> return (c,c))

-- | Writing runtime state of type @c@ in kernel code.
setEnv :: c -> Kernel sig c ()
setEnv c' = KC (\ c -> return ((),c'))

-- | Context switch to execute a kernel computation in user mode.
--
-- The 1st argument of type @Kernel sig c a@ is the kernel computation to be executed.
--
-- The 2nd argument of type @c@ is the initial value for runtime state.
--
-- The 3rd argument of type @a -> c -> User sig b@ is a finaliser for return values,
-- which also performs the context switch back to user mode.
kernel :: Kernel sig c a -> c -> (a -> c -> User sig b) -> User sig b
kernel (KC k) c f =
  UC (do (x,c') <- k c; let (UC m) = f x c' in m)

-- | Context switch to execute a user computation in kernel mode.
--
-- The 1st argument of type @User sig a@ is the user computation to be executed.
--
-- The 2nd argument of type @a -> Kernel sig c b@ is a finaliser for return values,
-- which also performs the context switch back to kernel mode.
user :: User sig a -> (a -> Kernel sig c b) -> Kernel sig c b
user (UC m) f =
  KC (\ c -> do x <- m; let (KC k) = f x in k c)

-- | Type of effectful runners that implement co-operations for the effects in signature @sig@,
-- where each of the co-operations is a kernel computation that can perform algebraic operations
-- given by (external) effects in the signature @sig'@ and access runtime state of type @c@.
--
-- Given an effect @eff :: * -> *@, the corresponding co-operations are given by a
-- function of type
--
-- > forall b . eff b -> Kernel sig' c b
--
-- in other words, by a mapping of every algebraic operation of the effect @eff@
-- (i.e., each of its constructors) to a corresponding kernel computation.
data Runner sig sig' c where
  Empty :: Runner '[] sig' c
  CoOps :: (forall b . eff b -> Kernel sig' c b)
        -> Runner sig sig' c-> Runner (eff ': sig) sig' c

-- | Make a runner for a single effect @eff@ by providing co-operations implementing
-- each of its algebraic operations.
--
-- For instance, a runner whose runtime state carries a file handle and that
-- implements a co-operation for write-only file access could be given by
--
-- > mkRunner (\ (Write s) -> do fh <- getEnv; performK (WriteFile fh s)) :: Runner '[WriteIO] '[FileIO] Handle
--
-- where we assume that the effect @eff@ is given by
--
-- > data WriteIO :: * -> * where
-- >   Write :: String -> FileIO ()
mkRunner :: (forall b . eff b -> Kernel sig c b) -> Runner '[eff] sig c
mkRunner coops = CoOps coops Empty

-- | Runner for the empty signature.
emptyRunner :: Runner '[] sig' c
emptyRunner = Empty

-- | The (disjoint) union of two signatures.
type family SigUnion (sig :: [* -> *]) (sig' :: [* -> *]) :: [* -> *] where
  SigUnion '[] sig' = sig'
  SigUnion (eff ': sig) sig' = eff ': (SigUnion sig sig')

-- | Taking the union of (the co-operations of) two runners with the same
-- external signature @sig''@ and runtime state @c@. The resulting runner
-- implements co-operations for the union of the given signatures.
--
-- The intended use of `unionRunners` is to build a runner for a composite
-- signature from runners for individual effects given by `mkRunner`.
unionRunners :: Runner sig sig'' c -> Runner sig' sig'' c
             -> Runner (SigUnion sig sig') sig'' c
unionRunners Empty r' = r'
unionRunners (CoOps coops r) r' =
  CoOps coops (unionRunners r r')

-- | Embedding a runner in a larger external signature.
embedRunner :: Runner sig sig' c -> Runner sig (eff ': sig') c
embedRunner Empty = Empty
embedRunner (CoOps coops r) =
  CoOps (\ op -> embedK (coops op)) (embedRunner r)

-- | Extending the runtime state with an additional component,
-- which the co-operations of the resulting runner keep unchanged.
extendRunner :: Runner sig sig' c' -> Runner sig sig' (c,c')
extendRunner Empty = Empty
extendRunner (CoOps coops r) =
  CoOps (\ op -> KC (\ (c,c') ->
                   do (x,c'') <- let (KC k) = coops op in k c';
                      return (x,(c,c''))))
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
pairRunners :: Runner sig sig'' c -> Runner sig' sig'' c'
            -> Runner (SigUnion sig sig') sig'' (c,c')
pairRunners Empty r' = extendRunner r'
pairRunners (CoOps coops r) r' =
  CoOps (\ op -> KC (\ (c,c') ->
                   do (x,c'') <- let (KC k) = coops op in k c
                      return (x,(c'',c'))))
        (pairRunners r r')

-- | Runner that forwards all of its co-operations to some enveloping runner.
fwdRunner :: Member eff sig => Runner '[eff] sig c
fwdRunner = CoOps genPerformK Empty

-- | Running a single algebraic operation as a kernel computation using the given runner.
runOp :: Runner sig sig' c -> Union sig b -> Kernel sig' c b
runOp Empty _ =
  error "this should not have happened"
runOp (CoOps coop coops) u =
  case decomp u of
    Right o -> coop o
    Left u -> runOp coops u

-- | Auxiliary operation for running user computations using a given runner in which
-- the initial runtime state is initialised by a value rather than an effectful user
-- computation as in `run`.
runAux :: Runner sig sig' c
       -> c
       -> User sig a
       -> (a -> c -> User sig' b)
       -> User sig' b
runAux r c (UC (Val x)) mf = mf x c
runAux r c (UC (E u q)) mf =
  kernel (runOp r u) c
        (\ x c' -> runAux r c' (UC (qApp q x)) mf)

-- | A programming construct to run user code using a runner with guaranteed finalisation.
--
-- The 1st argument (of type @Runner sig sig' c@) is the given runner.
--
-- The 2nd argument (of type @User sig' c@) is a user computation that produces an initial
-- value for the runtime state that the runner operates on.
--
-- The 3rd argument (of type @User sig a@) is the user computation that we are running
-- using the given runner. Observe that this user computation can only perform the
-- algebraic operations for the effects in the signature @sig@ implemented by the runner.
-- It cannot directly perform algebraic operations from the (external) signature @sig'@.
-- It can only do so if the runner explicitly forwards the needed algebraic operations.
--
-- The 4th argument (of type @a -> c -> User sig' b@) is a user computation that
-- finalises for return values. Notice that in addition to having access to return values,
-- it can also access the final value of the runtime state, so as to perform cleanup.
--
-- For instance, we can run a simple user computation using the write-only file access
-- runner defined in the description of `mkRunner` as follows
--
-- > run
-- >   (mkRunner (\ (Write s) -> do fh <- getEnv; performK (WriteFile fh s)))
-- >   (performU (OpenFile "hello.txt" WriteMode))
-- >   (do performU (Write "Hello, world."); performU (Write "Hello, again."))
-- >   (\ x fh -> do performU (CloseFile fh); return x)
-- >   :: User '[FileIO] () 
--
-- Here we initialise the runtime state for the write-only file access runner
-- with a file handle pointing to @"hello.txt"@, by performing a file open
-- operation that would be implemented by some enveloping runner. The user
-- computation that we run with said runner simply performs two calls to
-- the @Write@ operation. Finally, the finaliser closes the file handle and
-- passes on the return value unchanged. Observe how this write-only file
-- access runner hides the file handle from the user code being run (the 
-- latter can only perform the @Write@ operation that takes only a string
-- as an argument and not the file handle itself). Furthermore, the semantics
-- of the `run` operation ensures that the finaliser is always called exactly
-- once, ensuring a correct cleanup of the file handle resource, as desired.
--
-- In the context of the
-- talk [Interacting with external resources using runners (aka comodels)](https://danel.ahman.ee/talks/chocola19.pdf),
-- the `run` operation corresponds to the following programming construct
--
-- > using R @ M_init
-- > run M
-- > finally {
-- >   return x @ c -> M_return }
run :: Runner sig sig' c
    -> User sig' c
    -> User sig a
    -> (a -> c -> User sig' b)
    -> User sig' b
run r mi m mf =
  do c <- mi; runAux r c m mf

-- | A top-level for running user computations for the empty signature as pure, effect-free values.
pureTopLevel :: User '[] a -> a
pureTopLevel (UC (Val x)) = x
pureTopLevel _ = error "this should not have happened"

-- | A top-level for running user computations as Haskell's monadic computations.
topLevel :: Monad m => User '[m] a -> m a
topLevel (UC c) = runM c

-- | Syntactic sugar for top-level running of user computations in the IO monad, defined using `topLevel`.
ioTopLevel :: User '[IO] a -> IO a
ioTopLevel = topLevel
