{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : TwoLevelStateTest
Description : Example use of runners on a simple two-location integer-valued state
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides an example use of runners on a simple two-location 
integer-valued state, showing how runners can be used to "focus" on a 
fraction of some bigger external resource, in this case on one of the 
two memory locations of the `BigState` effect.
-}
module TwoLevelStateTest where

import Control.Runner

data BigState a where
  Get1 :: BigState Int
  Put1 :: Int -> BigState ()
  Get2 :: BigState String
  Put2 :: String -> BigState ()

data SmallState a where
  Get :: SmallState Int
  Put :: Int -> SmallState ()

smallStateCoOps :: SmallState r -> Kernel '[BigState] Int r
smallStateCoOps Get =
  do i <- getEnv;
     return i
smallStateCoOps (Put i) =
  setEnv i

smallStateRunner :: Runner '[SmallState] '[BigState] Int
smallStateRunner = mkRunner smallStateCoOps

smallStateInitially :: User '[BigState] Int
smallStateInitially =
  do i <- performU Get1
     return (i + 7)

smallStateFinally :: () -> Int -> User '[BigState] ()
smallStateFinally _ i = performU (Put1 i)

smallStateComp :: User '[BigState] ()
smallStateComp =
  run
    smallStateRunner
    smallStateInitially
    (
      do i <- performU Get;
         performU (Put (i + 42))
    )
    smallStateFinally

bigStateCoOps :: BigState r -> Kernel '[] (Int,String) r
bigStateCoOps Get1 =
  do (i,s) <- getEnv;
     return i
bigStateCoOps (Put1 i) =
  do (_,s) <- getEnv;
     setEnv (i,s)
bigStateCoOps Get2 =
  do (i,s) <- getEnv;
     return s
bigStateCoOps (Put2 s) =
  do (i,_) <- getEnv;
     setEnv (i,s)

bigStateRunner :: Runner '[BigState] '[] (Int,String)
bigStateRunner = mkRunner bigStateCoOps


bigStateInitially :: User '[] (Int,String)
bigStateInitially = return (0,"default value")

bigStateFinally :: () -> (Int,String) -> User '[] (Int,String)
bigStateFinally _ (i,s) = return (i,s)

bigStateComp :: User '[] (Int,String)
bigStateComp =
  run
    bigStateRunner
    bigStateInitially
    (
      do smallStateComp;
         performU (Put2 "new value")
    )
    bigStateFinally

test = pureTopLevel bigStateComp -- expected result (49,"new value")