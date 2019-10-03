{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

--
-- Experiments with a runner for simple 2-level state, 
-- where the bigger state has 2 locations and the smaller
-- state has 1 location, and a runner mediates between them.
--

module TwoLevelStateTest where

import Control.Monad.Runner

data BigState :: * -> * where
  Get1 :: BigState Int
  Put1 :: Int -> BigState ()
  Get2 :: BigState String
  Put2 :: String -> BigState ()

data SmallState :: * -> * where
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

test = pureTopLevel bigStateComp