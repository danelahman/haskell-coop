{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

--
--
--

{-
module AmbientTests where

import Control.Monad.Runner
import Control.Monad.Runner.Ambients

ambFun1 :: AmbVal Int -> Int -> User '[Amb] Int
ambFun1 x y =
  do x <- get x;
     return (x + y)

test1 :: User '[Amb] Int
test1 =
  withAmbVal
    (4 :: Int)
    (\ x ->
      withAmbFun
        (ambFun1 x)
        (\ f ->
          do x <- rebindVal x 2;
             r <- apply f 1;
             r))

test2 = ambTopLevel test1
-}