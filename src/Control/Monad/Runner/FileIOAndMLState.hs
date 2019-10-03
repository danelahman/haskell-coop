{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

--
-- Combination of the runners for File IO and ML-style state.
--

module Control.Monad.Runner.FileIOAndMLState
  (
  withFile, ioMltopLevel
  ) where

import Control.Monad.Runner
import Control.Monad.Runner.FileIO hiding (withFile)
import Control.Monad.Runner.MLState hiding (mlTopLevel)

import System.IO hiding (withFile)

--
-- With-file construct in the presence of ML-style state.
--
-- Currently limited to the use of one file at a time.
--
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

--
-- Top-level running of file IO plus ML-style state.
--
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
