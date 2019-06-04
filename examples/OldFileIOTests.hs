{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for the file-comodels in OldFileIO.
--

module OldFileIOTests where

import Control.Monad.Comodel
import Control.Monad.Comodel.OldFileIO

import System.IO

test1 :: Comp '[File] ()
test1 =
  do s <- focus (perform Read);
     if s == "foo"
     then (focus (perform (Write "contents was foo")))
     else (focus (perform (Write "contents was not foo")))

writeLines :: Member File iface => [String] -> Comp iface ()
writeLines [] = return ()
writeLines (l:ls) = do _ <- focus (perform (Write l));
                       focus (perform (Write "\n"));
                       writeLines ls

exampleLines = ["Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                "Cras sit amet felis arcu.",
                "Maecenas ac mollis mauris, vel fermentum nibh."]

test2 :: Comp '[IO] ()
test2 =                                         -- in IO context
  run
    fioComodel
    ioFioLens
    (                                           -- in FileIO context
      run
        fhComodel
        (fioFhLens "./out.txt")
        (                                       -- in File context, with FH comodel
          writeLines exampleLines               -- this comodel appends to the existing file
        )
    )

test3 = runIO test2

test4 :: Comp '[IO] ()
test4 =                                         -- in IO context
  run
    fioComodel
    ioFioLens
    (                                           -- in FileIO context
      run
        fcComodel
        (fioFcLens "./out2.txt")
        (                                       -- in File context, with FC comodel
          writeLines exampleLines               -- this comodel appends to the existing file
        )
    )

test5 = runIO test4

test6 :: Comp '[IO] ()
test6 =                                         -- in IO context
  run
    fioComodel
    ioFioLens
    (                                           -- in FileIO context
      run
        fcOwComodel
        (fioFcOwLens "./out3.txt")
        (                                       -- in File context, with FC+OW comodel
          writeLines exampleLines               -- this comodel overwrites the existing file
        )
    )

test7 = runIO test6

test8 :: Comp '[IO] ()
test8 =                                         -- in IO context
  run
    fioComodel
    ioFioLens
    (                                           -- in FileIO context
      run
        fcOwComodel
        (fioFcOwLens "./out4.txt")
        (                                       -- in File context, with FC+OW comodel
          do s <- focus (perform Read);
             focus (perform (Write s));
             focus (perform (Write "\n"));
             focus (perform (Write s));
             if not (s == "foo")
             then (do _ <- focus (perform Clean);    -- selectively empties file contents
                      focus (perform (Write "foo"))) 
             else (return ())
        )
    )

test9 = runIO test8