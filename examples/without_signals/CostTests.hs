{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Module      : IntStateTests
Description : Example use cases of the runner for cost model instrumentation from `Control.Runner.Cost`
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides example use cases of the runner for 
cost model instrumentation from `Control.Runner.Cost`.
-}
module CostTests where

import Control.Runner
import Control.Runner.Cost

import Control.Runner.MLState

import Control.Runner.FileIO
import System.IO hiding (withFile)


-- Instrumenting ML-style state with a simple cost model

test1 :: Int -> Int -> User '[MLState] (Int,Int)
test1 x y =
  do r <- alloc x;
     r' <- alloc y;
     x' <- (!) r;
     y' <- (!) r';
     return (x',y')

test2 =
  mlTopLevel (costInstrumentation (test1 4 2)) -- expected result ((4,2),4)                            

test3 :: Int -> User '[MLState] Int
test3 x =
  do r <- alloc x;
     r =:=  (x + 2);
     y <- (!) r;
     return y

test4 = mlTopLevel (costInstrumentation (test3 4)) -- expected result (6,3)

test5 :: String -> (String -> Int) -> User '[MLState] Int
test5 s f =
  do r <- alloc f;  -- storing a higher-order (pure) function argument in the state
     x <- test3 42;
     g <- (!) r;
     return (g s + x) -- length s + 44

test6 = mlTopLevel (costInstrumentation (test5 "foobar" length)) -- expected result (50,5)
                                                                 -- 2 operation calls from test5,
                                                                 -- and 3 operation calls from test3


-- Instrumenting file IO state with a simple cost model

writeLines :: Member File sig => [String] -> User sig ()
writeLines [] = return ()
writeLines (l:ls) = do fWrite l;
                       fWrite "\n";
                       writeLines ls

exampleLines = ["Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                "Cras sit amet felis arcu.",
                "Maecenas ac mollis mauris, vel fermentum nibh."]

test7 :: User '[IO] ()
test7 =                                   -- in IO signature, using IO container
  run
    fioRunner
    ioFioInitialiser
    (                                     -- in FileIO signature, using FIO runner
      run
        fhRunner
        (fioFhInitialiser "./out.txt")
        (                                 -- in File signature, using FH runner
          writeLines exampleLines
        )
        fioFhFinaliser
    )
    ioFioFinaliser

test8 = ioTopLevel (costInstrumentation test7) -- expected result ((),11)
                                               -- 6 operation calls from writeLines
                                               -- 4 operation calls from fioFhInitialiser
                                               -- 1 operation call from fioFhFinaliser
                                               -- 0 operation calls from ioFioInitialiser and ioFioFinaliser
