{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for the file-runners in OldFileIO.
--

module OldFileIOTests where

import Control.Monad.Runner
import Control.Monad.Runner.OldFileIO

import System.IO

test1 :: User '[File] ()
test1 =
  do s <- focus (performU Read);
     if s == "foo"
     then (focus (performU (Write "contents was foo")))
     else (focus (performU (Write "contents was not foo")))

writeLines :: Member File iface => [String] -> User iface ()
writeLines [] = return ()
writeLines (l:ls) = do _ <- focus (performU (Write l));
                       focus (performU (Write "\n"));
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
          do s <- focus (performU Read);
             focus (performU (Write s));
             focus (performU (Write "\n"));
             focus (performU (Write s));
             if not (s == "foo")
             then (do _ <- focus (performU Clean);    -- selectively empties file contents
                      focus (performU (Write "foo"))) 
             else (return ())
        )
        (fioFcOwFinaliser "./out4.txt")
    )
    ioFioFinaliser

test9 = ioTopLevel test8
