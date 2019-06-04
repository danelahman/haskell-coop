{-# LANGUAGE DataKinds #-}

--
-- Example tests for the combination of file IO and ML-style state.
--

module FileIOAndMLStateTests where

import Control.Monad.Comodel
import Control.Monad.Comodel.FileIO hiding (withFile)
import Control.Monad.Comodel.FileIOAndMLState
import Control.Monad.Comodel.MLState hiding (topLevel)

import Data.Typeable

test1 :: FilePath -> Comp '[IO,MLState] String
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
     
test2 = topLevel (test1 "./out.txt")