{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Control.Runner.FileIOAndMLState
Description : Combination of file IO and ML-style state, using the horizontal composition of runners
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module uses the horizontal combination of runners (`pairRunners`) to combine 
the file IO runners from `FileIO` and the ML-style state runner from `MLState`.
-}
module Control.Runner.FileIOAndMLState
  (
  withFile, ioMltopLevel
  ) where

import Control.Runner
import Control.Runner.FileIO hiding (withFile)
import Control.Runner.MLState hiding (mlTopLevel)

import System.IO hiding (withFile)

-- | A variant of the with-file construct that runs user code that can
-- perform effects both from the `File` and `MLState` effects. For
-- simplicity, currently limited to the use of one file at a time.
withFile :: FilePath -> User '[File,MLState] a -> User '[IO,MLState] a
withFile fn c =
  run
    (pairRunners fioRunner fwdRunner)
    (return ((),()))
    (
      run
        (pairRunners fhRunner (fwdRunner :: Runner '[MLState] '[FileIO,MLState] ()))
        (do s <- fioFhInitialiser fn;
            return (s,()))
        c
        (\ x (s,()) -> fioFhFinaliser x s)
    )
    (\ x _ -> return x)

-- | Top level for running user code that can perform
-- both `IO` and `MLState` effects.
ioMltopLevel :: User '[IO,MLState] a -> IO a
ioMltopLevel m =
  ioTopLevel
    (
      run
        (pairRunners fwdRunner mlRunner)
        (do h <- mlInitialiser;
            return ((),h))
        m
        (\ x _ -> return x)
    )
