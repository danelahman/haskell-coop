{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

--
--
--

module AmbientTests where

import Control.Monad.Runner
import Control.Monad.Runner.Ambients

ambFun1 :: AmbVal Int -> Int -> AmbEff Int
ambFun1 x y =
  do x <- getVal x;
     return (x + y)

test1 :: AmbEff Int
test1 =
  withAmbVal
    (4 :: Int)
    (\ x ->
      withAmbFun
        (ambFun1 x)
        (\ f ->
          do _ <- rebindVal x 2;
             applyFun f 1))

test2 = ambTopLevel test1