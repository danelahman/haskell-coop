{-# LANGUAGE DataKinds #-}

{-|
Module      : FileIOAndMLStateTests
Description : Example use cases of the combination of file IO and ML-style state from `Control.Runner.FileIOAndMLState`
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides some example use cases of the combination of 
file IO and ML-style state from `Control.Runner.FileIOAndMLState`.
-}
module FileIOAndMLStateTests where

import Control.Runner
import Control.Runner.FileIO hiding (withFile)
import Control.Runner.FileIOAndMLState
import Control.Runner.MLState hiding (mlTopLevel)

import Data.Typeable

test1 :: FilePath -> User '[IO,MLState] String
test1 fn =
  do r <- alloc "";
     withFile
       fn
       (
         do s <- fRead;
            r =:= s;
            fWrite s      -- to retain the file's original contents
       );
     s <- (!) r;
     withFile
       fn
       (
         fWrite (s ++ "foobar")     -- updating the file's contents
       );
     s' <- (!) r;
     return s'
     
test2 = ioMltopLevel (test1 "./out.txt")