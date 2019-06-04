{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.Comodel.FileIOAndMLState (withFile, topLevel) where

import Control.Monad.Comodel
import Control.Monad.Comodel.FileIO hiding (withFile)
import Control.Monad.Comodel.MLState hiding (topLevel)

import System.IO hiding (withFile)

--
-- With-file construct in the presence of ML-style state.
--
-- Currently limited to the use of one file at a time.
--
withFile :: FilePath -> Comp '[File,MLState] a -> Comp '[IO,MLState] a
withFile fn c =
  run
    (pairComodels fioComodel fwdComodel)
    (pairIFLenses ioFioLens fwdIFLens)
    (
      run
        (pairComodels fhComodel (fwdComodel :: Comodel '[FileIO,MLState] '[MLState] ()))
        (pairIFLenses (fioFhLens fn) fwdIFLens)
        c
    )

--
-- Top-level running of file IO plus ML-style state.
--
topLevel :: Comp '[IO,MLState] a -> IO a
topLevel c =
  runIO
    (
      run
        (pairComodels fwdComodel mlComodel)
        (pairIFLenses fwdIFLens mlLens)
        c
    )

