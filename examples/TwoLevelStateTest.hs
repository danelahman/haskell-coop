{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module TwoLevelStateTest where

import Control.Monad.Comodel

data BigState :: * -> * where
  Get1 :: BigState Int
  Put1 :: Int -> BigState ()
  Get2 :: BigState String
  Put2 :: String -> BigState ()

data SmallState :: * -> * where
  Get :: SmallState Int
  Put :: Int -> SmallState ()

smallStateCoOps :: SmallState r -> Int -> Comp '[BigState] (r,Int)
smallStateCoOps Get     i = return (i ,i)
smallStateCoOps (Put i) _ = return ((),i)

smallStateComodel :: Comodel '[BigState] '[SmallState] Int
smallStateComodel = mkComodel smallStateCoOps

smallStateInitially :: Comp '[BigState] Int
smallStateInitially =
  do i <- perform Get1
     return (i + 7)

smallStateFinally :: Int -> () -> Comp '[BigState] ()
smallStateFinally i _ = perform (Put1 i)

bigStateSmallStateLens :: IFLens '[BigState] Int () ()
bigStateSmallStateLens = mkIFLens smallStateInitially smallStateFinally

smallStateComp :: Comp '[BigState] ()
smallStateComp =
  run
    smallStateComodel
    bigStateSmallStateLens
    (
      do i <- perform Get;
         perform (Put (i + 42))
    )

bigStateCoOps :: BigState r -> (Int,String) -> Comp '[] (r,(Int,String))
bigStateCoOps Get1     (i,s) = return (i ,(i,s))
bigStateCoOps (Put1 i) (_,s) = return ((),(i,s))
bigStateCoOps Get2     (i,s) = return (s ,(i,s))
bigStateCoOps (Put2 s) (i,_) = return ((),(i,s))

bigStateComodel :: Comodel '[] '[BigState] (Int,String)
bigStateComodel = mkComodel bigStateCoOps

bigStateInitially :: Comp '[] (Int,String)
bigStateInitially = return (0,"default value")

bigStateFinally :: (Int,String) -> () -> Comp '[] (Int,String)
bigStateFinally (i,s) _ = return (i,s)

pureBigStateLens :: IFLens '[] (Int,String) () (Int,String)
pureBigStateLens = mkIFLens bigStateInitially bigStateFinally

bigStateComp :: Comp '[] (Int,String)
bigStateComp =
  run
    bigStateComodel
    pureBigStateLens
    (
      do smallStateComp;
         perform (Put2 "new value")
    )

test = runPure bigStateComp