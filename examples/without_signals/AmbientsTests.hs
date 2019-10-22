{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : AmbientsTests
Description : Example use cases of the runner for ambient values and ambient functions from `Control.Runner.Ambients`
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides some example use cases of the runner for
ambient values and ambient functions from `Control.Runner.Ambients`.
-}
module AmbientsTests where

import Control.Runner
import Control.Runner.Ambients

ambFun :: AmbVal Int -> Int -> AmbEff Int
ambFun x y =
  do x <- getVal x;
     return (x + y)

test1 :: AmbEff Int
test1 =
  withAmbVal
    (4 :: Int)
    (\ x ->                             -- bind an ambient value `x` with (initial) value 4     
      withAmbFun
        (ambFun x)
        (\ f ->                         -- bind an ambient function `f` using the ambient value `x`
          do rebindVal x 2;             -- rebind/update the ambient value `x` with value 2
             applyFun f 1))

test2 = ambTopLevel test1 -- expected result 5

test3 :: AmbEff Int
test3 =
  withAmbVal
    (4 :: Int)
    (\ x ->
      withAmbFun
        (ambFun x)
        (\ f ->
          do rebindVal x 2;
             rebindFun f (ambFun x);
             (applyFun f 1)))

test4 = ambTopLevel test3 -- expected result 3

test5 :: AmbEff Int
test5 =
  withAmbVal
    (4 :: Int)
    (\ x ->
      withAmbFun
        (ambFun x)
        (\ f ->
          do rebindVal x 2;
             withAmbFun
               (ambFun x)
               (\ g -> applyFun f 1)))

test6 = ambTopLevel test5 -- expected result 5