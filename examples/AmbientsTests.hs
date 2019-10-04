{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

--
-- Tests for ambient values and ambient functions implemented as a runner in Ambients.
--

module AmbientsTests where

import Control.Monad.Runner
import Control.Monad.Runner.Ambients

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
             applyFun f 1))             -- call the ambient function `f`, it will use `x` with value 4 (at `f`'s definition site)

test2 = ambTopLevel test1               -- expected result 5

test3 :: AmbEff Int
test3 =
  withAmbVal
    (4 :: Int)
    (\ x ->
      withAmbFun
        (ambFun x)
        (\ f ->
          do rebindVal x 2;
             withAmbFun
               (ambFun x)
               (\ g -> applyFun g 1)))

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