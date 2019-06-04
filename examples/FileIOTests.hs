{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for the file-comodels in FileIO.
--

module FileIOTests where

import Control.Monad.Comodel
import Control.Monad.Comodel.FileIO

import System.IO hiding (withFile)

writeLines :: Member File iface => [String] -> Comp iface ()
writeLines [] = return ()
writeLines (l:ls) = do _ <- fWrite l;
                       fWrite "\n";
                       writeLines ls

exampleLines = ["Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                "Cras sit amet felis arcu.",
                "Maecenas ac mollis mauris, vel fermentum nibh."]


test1 :: Comp '[IO] ()
test1 =                                   -- in IO context, using IO comodel/monad
  run
    fioComodel
    ioFioLens
    (                                     -- in FileIO context, using FIO comodel
      run
        fhComodel
        (fioFhLens "./out.txt")
        (                                 -- in File context, using FH comodel
          writeLines exampleLines
        )
    )

test2 = runIO test1

test3 :: Comp '[IO] ()
test3 =                                   -- in IO context, using IO comodel/monad
  run
    fioComodel
    ioFioLens
    (                                     -- in FileIO context, using FIO comodel
      run
        fhComodel
        (fioFhLens "./out2.txt")
        (                                 -- in File context, using FH comodel
          do s <- fRead;                  -- contents of the file at the point of entering the FH comodel
             fWrite s;                    -- write the contents back to the file
             writeLines exampleLines      -- write additional lines to the file
        )
    )

test4 = runIO test3

test5 :: Comp '[IO] ()
test5 =                                            -- in IO context, using IO comodel/monad
  run
    fioComodel
    ioFioLens
    (                                              -- in FileIO context, using FIO comodel
      run
        fhComodel
        (fioFhLens "./out3.txt")
        (                                          -- in File context, using FH comodel
          do _ <- run
                    fcComodel
                    fhFcLens
                    (                              -- in File context, using FC comodel
                      writeLines exampleLines      -- writing example lines using FC comodel
                    );
             fWrite "Proin eu porttitor enim."     -- writing another line using FH comodel
        )
    )

test6 = runIO test5

test7 :: Comp '[IO] ()
test7 =                                                     -- in IO context, using IO comodel/monad
  withFile
    "./out4.txt"
    (                                                       -- in File context, using FIO,FH,FC comodels
      do _ <- writeLines exampleLines;
         fWrite "\nProin eu porttitor enim.\n\n";
         s <- fRead;                                        -- the initial contents of the file
         fWrite "\nInitial contents of the file [START]\n";
         fWrite s;
         fWrite "\nInitial contents of the file [END]\n"
    )

test8 = runIO test7