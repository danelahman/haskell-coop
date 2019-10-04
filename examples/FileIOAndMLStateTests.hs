{-# LANGUAGE DataKinds #-}

--
-- Example tests for the combination of file IO and ML-style state.
--

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