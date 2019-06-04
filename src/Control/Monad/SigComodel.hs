{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.SigComodel (
  Comp, raise, tryUnless,
  
  Comodel, IfaceUnion, emptyComodel, mkComodel, unionComodels,
  
  IFLens, mkIFLens, initializer, finalizerRet, finalizerSig,
  
  run, perform, runPure, runIO,

  Member
  ) where

--
-- Tested with
--   http://hackage.haskell.org/package/freer-simple v1.2.1.0
-- variant of the Freer monad.
--
import Control.Monad.Freer.Internal hiding (run,raise)

import Control.Monad.Except

--
-- Signal codes are ordinary integers (for simplicity).
--
type SigCode = Int

--
-- Computations waiting to be run in some external world given
-- by some comodel providing co-operations for interface iface.
-- They are defined by piggybacking on the Freer monad.
-- As in the Freer monad, iface is a list of effects, each
-- represented by a type of kind `* -> *`, e.g., `State s`.
--
-- Using Except explicitly as opposed to using ExceptT makes
-- some of the functions below slightly easier to define.
--
newtype Comp iface a = C (Eff iface (Except SigCode a))

instance Functor (Comp iface) where
  fmap f (C c) = C (fmap (fmap f) c)

instance Applicative (Comp iface) where
  pure x = C (pure (pure x))
  (C f) <*> (C c) =  C ((fmap (<*>) f) <*> c)

instance Monad (Comp iface) where
  (C c) >>= f =
    C (c >>= (\ c' ->
      case (runExcept c') of
        Right x  -> let (C y) = f x in y
        Left sig -> return (throwError sig)))

--
-- Performing operations while focussed on a single effect.
--
-- If working with a bigger interface, first focus on a single
-- effect before calling perform. This is a small price to
-- pay to get more intricate GADT-based interfaces (such as
-- various kinds of state) to work conveniently. In most cases
-- focussing is limited to defining derived generic effects.
--
perform :: e r -> Comp '[e] r
perform o = C (do x <- send o; return (return x))

--
-- Generic perform function used internally in this module.
--
genPerform :: Member e iface => e r -> Comp iface r
genPerform o = C (do x <- send o; return (return x))

--
-- Raising a signal.
--
raise :: SigCode -> Comp iface a
raise sig = C (return (throwError sig))

--
-- Handling (user-raised) signals (using exceptional syntax).
--
tryUnless :: Comp iface a -> (a -> Comp iface b) -> (SigCode -> Comp iface b) -> Comp iface b
tryUnless (C c) f g =
  C (do c' <- c;
        case (runExcept c') of
          Right x  -> case (f x) of C c -> c
          Left sig -> case (g sig) of C c -> c)

--
-- A comodel is simply a list of co-operations, where the first
-- argument of CoOps requires a co-operation for each operation
-- in the given effect `e :: * -> *`, e.g., for `Get` and `Put`.
--
data Comodel iface iface' w where
  Empty :: Comodel iface '[] w
  CoOps :: (forall b . e b -> w -> Comp iface (b,w))
        -> Comodel iface iface' w -> Comodel iface (e ': iface') w

mkComodel :: (forall b . e b -> w -> Comp iface (b,w)) -> Comodel iface '[e] w
mkComodel coops = CoOps coops Empty

--
-- (Effectful) initialization/finalization/signal-handling lenses.
--
data IFLens iface w a b = IFL { initially  :: Comp iface w,
                                finallyRet :: w -> a -> Comp iface b,
                                finallySig :: w -> SigCode -> Comp iface b}

mkIFLens :: Comp iface w -> (w -> a -> Comp iface b)
         -> (w -> SigCode -> Comp iface b) -> IFLens iface w a b
mkIFLens ci cfr cfs =
  IFL { initially = ci , finallyRet = cfr , finallySig = cfs }

initializer :: IFLens iface w a b -> Comp iface w
initializer l = initially l

finalizerRet :: IFLens iface w a b -> w -> a -> Comp iface b
finalizerRet l = finallyRet l

finalizerSig :: IFLens iface w a b -> w -> SigCode -> Comp iface b
finalizerSig l = finallySig l

--
-- The
--
--  using C @ c_init
--  run c
--  finally @ w {
--    return(x) -> c_fin,
--    signal(s) -> c_sig }
--
-- construct for initializing, running,
-- finalizing, and signal-handling a computation.
--
runOp :: Comodel iface iface' w -> Union iface' b -> w -> Eff iface (Except SigCode (b,w))
runOp Empty _ _ =
  error "this should not have happened"
runOp (CoOps coop coops) u w =
  case decomp u of
    Right o -> case (coop o w) of C c -> c
    Left u  -> runOp coops u w

runComp :: Comodel iface iface' w -> Comp iface' a -> w -> Eff iface (Except SigCode a,w)
runComp comod (C (Val c)) w =
  case (runExcept c) of
    Right x  -> return (return x,w)
    Left sig -> return (throwError sig,w)
runComp comod (C (E u q)) w =
  do c <- runOp comod u w;
     case (runExcept c) of
       Right (x,w') -> runComp comod (C (qApp q x)) w'
       Left sig     -> return (throwError sig,w)

run :: Comodel iface iface' w -> IFLens iface w a b -> Comp iface' a -> Comp iface b
run comod l c =
  do w <- initially l;
     C (do (c',w') <- runComp comod c w;
           case (runExcept c') of
             Right x  -> case (finallyRet l w' x) of (C c) -> c
             Left sig -> case (finallySig l w' sig) of (C c) -> c)

--
-- Running computation in top-level pure and IO worlds.
--
-- Gives a runtime error when the program was not signal-free.
--
runPure :: Comp '[] a -> a
runPure (C (Val c)) =
  case (runExcept c) of
    Right x -> x
    Left  _ -> error "program was not signal-free"
runPure _ = error "this should not have happened"

runIO :: Comp '[IO] a -> IO a
runIO (C c) =
  do c' <- runM c;
     case (runExcept c') of
       Right x -> return x
       Left  _ -> error "program was not signal-free"

--
-- Empty and union of comodels.
--
emptyComodel :: Comodel iface '[] w
emptyComodel = Empty

type family IfaceUnion (iface :: [* -> *]) (iface' :: [* -> *]) :: [* -> *] where 
  IfaceUnion '[] iface' = iface'
  IfaceUnion (e ': iface) iface' = e ': (IfaceUnion iface iface')

unionComodels :: Comodel iface iface' w -> Comodel iface iface'' w
              -> Comodel iface (IfaceUnion iface' iface'') w
unionComodels Empty comod' = comod'
unionComodels (CoOps coops comod) comod' =
  CoOps coops (unionComodels comod comod')

--
-- Trivial forwarding init-fin lens for when the comodel carrier is trivial.
--
fwdIFLens :: IFLens iface () a a
fwdIFLens = mkIFLens (return ()) (\ _ x -> return x) (\ _ sig -> raise sig)

--
-- Focussing on a particular effect in a larger external world.
--
focusCoOps :: Member e iface => e r -> () -> Comp iface (r,())
focusCoOps o _ =
  do x <- genPerform o;
     return (x,())

focusComodel :: Member e iface => Comodel iface '[e] ()
focusComodel = mkComodel focusCoOps

focus :: Member e iface => Comp '[e] a -> Comp iface a
focus c =
  run
    focusComodel
    fwdIFLens
    c
