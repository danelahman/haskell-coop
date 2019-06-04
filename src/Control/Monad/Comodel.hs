{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Comodel (
  Comp, embed,
  
  Comodel, IfaceUnion, emptyComodel, mkComodel, unionComodels,
  embedComodel, extendComodel, pairComodels, fwdComodel,
  
  IFLens, mkIFLens, initializer, finalizer,
  fwdIFLens, embedIFLens, pairIFLenses, 
  
  run, perform, focus, runPure, runIO,

  Member
  ) where

--
-- Tested with
--   http://hackage.haskell.org/package/freer-simple v1.2.1.0
-- variant of the Freer monad.
--
import Control.Monad.Freer.Internal hiding (run)

--
-- Computations waiting to be run in some external world given
-- by some comodel providing co-operations for interface iface.
-- They are defined by piggybacking on the Freer monad.
-- As in the Freer monad, iface is a list of effects, each
-- represented by a type of kind `* -> *`, e.g., `State s`.
--
-- The type `Comp iface a` captures the typing judgement
--
--   Gamma  |-^iface  M  :  a
--
newtype Comp iface a =
  C (Eff iface a) deriving (Functor,Applicative,Monad)

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
perform o = C (send o)

--
-- Generic perform function used internally in this module.
--
genPerform :: Member e iface => e r -> Comp iface r
genPerform o = C (send o)

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
-- (Effectful) initialization-finalization lenses.
--
data IFLens iface w a b = IFL { initially :: Comp iface w,
                                finally   :: w -> a -> Comp iface b }

mkIFLens :: Comp iface w -> (w -> a -> Comp iface b) -> IFLens iface w a b
mkIFLens ci cf = IFL { initially = ci , finally = cf }

initializer :: IFLens iface w a b -> Comp iface w
initializer l = initially l

finalizer :: IFLens iface w a b -> w -> a -> Comp iface b
finalizer l = finally l

--
-- The
--
--  using C @ c_init
--  run c
--  finally @ w {
--    return(x) -> c_fin }
--
-- construct for initializing, running, and finalizing a computation.
--
runOp :: Comodel iface iface' w -> Union iface' b -> w -> Comp iface (b,w)
runOp Empty _ _ =
  error "this should not have happened"
runOp (CoOps coop coops) u w =
  case decomp u of
    Right o -> coop o w
    Left u -> runOp coops u w

runComp :: Comodel iface iface' w -> Comp iface' a -> w -> Comp iface (a,w)
runComp _     (C (Val x)) w = C (Val (x,w))
runComp comod (C (E u q)) w =
  do (x,w') <- runOp comod u w;
     runComp comod (C (qApp q x)) w'

run :: Comodel iface iface' w -> IFLens iface w a b -> Comp iface' a -> Comp iface b
run comod l c =
  do w      <- initially l;
     (x,w') <- runComp comod c w;
     finally l w' x

--
-- Running computation in a top-level pure and IO worlds.
--
runPure :: Comp '[] a -> a
runPure (C (Val x)) = x
runPure _ = error "this should not have happened"

runIO :: Comp '[IO] a -> IO a
runIO (C c) = runM c

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
-- Embedding a comodel in a larger external world.
--
embedComodel :: Comodel iface iface' w -> Comodel (e ': iface) iface' w
embedComodel Empty = Empty
embedComodel (CoOps coops comod) =
  CoOps (\ o w -> embed (coops o w)) (embedComodel comod)

--
-- Extending the carrier of a comodel with some type/set.
--
extendComodel :: Comodel iface iface' w' -> Comodel iface iface' (w,w')
extendComodel Empty = Empty
extendComodel (CoOps coops comod) =
  CoOps (\ o (w,w') -> do (y,w'') <- coops o w';
                          return (y,(w,w'')))
        (extendComodel comod)

--
-- Pairing two comodels in the same external world.
--
pairComodels :: Comodel iface iface' w -> Comodel iface iface'' w'
             -> Comodel iface (IfaceUnion iface' iface'') (w,w')
pairComodels Empty comod' = extendComodel comod'
pairComodels (CoOps coops comod) comod' =
  CoOps (\ o (w,w') -> do (y,w'') <- coops o w;
                          return (y,(w'',w')))
        (pairComodels comod comod')

--
-- Comodel that forwards all co-operations to the external world.
--
fwdComodel :: Member e iface => Comodel iface '[e] w
fwdComodel = CoOps (\ o w -> do x <- genPerform o;
                                return (x,w))
                   Empty

--
-- Trivial forwarding init-fin lens for when the comodel carrier is trivial.
--
fwdIFLens :: IFLens iface () a a
fwdIFLens = mkIFLens (return ()) (\ _ x -> return x)

--
-- Embedding an initialization-finalization lens in a larger external world.
--
embedIFLens :: IFLens iface w a b -> IFLens (e ': iface) w a b
embedIFLens l = mkIFLens (embed (initializer l))
                         (\ w x -> embed (finalizer l w x))

--
-- Pairing two initialization-finalization lenses in the same external world.
--
pairInitially :: (Comp iface w) -> (Comp iface w') -> Comp iface (w,w')
pairInitially c c' =
  do w <- c;
     w' <- c';
     return (w,w')

pairFinally :: (w -> a -> Comp iface b)
            -> (w' -> b -> Comp iface c)
            -> ((w,w') -> a -> Comp iface c)
pairFinally c c' (w,w') x =
  do y <- c w x; c' w' y

pairIFLenses :: IFLens iface w a b
             -> IFLens iface w' b c
             -> IFLens iface (w,w') a c
pairIFLenses l l' =
  mkIFLens (pairInitially (initializer l) (initializer l'))
           (pairFinally (finalizer l) (finalizer l'))


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

--
-- Embedding a smaller external world into a larger world.
--
embed :: Comp iface a -> Comp (e ': iface) a
embed (C c) = C (raise c)
