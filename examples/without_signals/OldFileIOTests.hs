{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : OldFileIOTests
Description : Example use cases of the runners for file IO from `Control.Runner.OldFileIO`
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides some example use cases of the 
runners for file IO from `Control.Runner.OldFileIO`.
-}
module OldFileIOTests where

import Control.Runner
import Control.Runner.OldFileIO

import System.IO

test1 :: User '[File] ()
test1 =
  do s <- performU Read;
     if s == "foo"
     then (performU (Write "contents was foo"))
     else (performU (Write "contents was not foo"))

writeLines :: Member File iface => [String] -> User iface ()
writeLines [] = return ()
writeLines (l:ls) = do performU (Write l);
                       performU (Write "\n");
                       writeLines ls

exampleLines = ["Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                "Cras sit amet felis arcu.",
                "Maecenas ac mollis mauris, vel fermentum nibh."]

test2 :: User '[IO] ()
test2 =                                         -- in IO signature
  run
    fioRunner
    ioFioInitialiser
    (                                           -- in FileIO signature
      run
        fhRunner
        (fioFhInitialiser "./out.txt")
        (                                       -- in File signature, with FH runner
          writeLines exampleLines               -- this runner appends to the existing file
        )
        fioFhFinaliser
    )
    ioFioFinaliser

test3 = ioTopLevel test2

test4 :: User '[IO] ()
test4 =                                         -- in IO signature
  run
    fioRunner
    ioFioInitialiser
    (                                           -- in FileIO signature
      run
        fcRunner
        (fioFcInitialiser "./out2.txt")
        (                                       -- in File signature, with FC runner
          writeLines exampleLines               -- this runner appends to the existing file
        )
        (fioFcFinaliser "./out2.txt")
    )
    ioFioFinaliser

test5 = ioTopLevel test4

test6 :: User '[IO] ()
test6 =                                         -- in IO signature
  run
    fioRunner
    ioFioInitialiser
    (                                           -- in FileIO signature
      run
        fcOwRunner
        (fioFcOwInitialiser "./out3.txt")
        (                                       -- in File signature, with FC+OW runner
          writeLines exampleLines               -- this runner overwrites the existing file
        )
        (fioFcOwFinaliser "./out3.txt")
    )
    ioFioFinaliser

test7 = ioTopLevel test6

test8 :: User '[IO] ()
test8 =                                         -- in IO signature
  run
    fioRunner
    ioFioInitialiser
    (                                           -- in FileIO signature
      run
        fcOwRunner
        (fioFcOwInitialiser "./out4.txt")
        (                                       -- in File signature, with FC+OW runner
          do s <- performU Read;
             performU (Write s);
             performU (Write "\n");
             performU (Write s);
             if not (s == "foo")
             then (do performU Clean;   -- selectively empties file contents
                      performU (Write "foo")) 
             else (return ())
        )
        (fioFcOwFinaliser "./out4.txt")
    )
    ioFioFinaliser

test9 = ioTopLevel test8
