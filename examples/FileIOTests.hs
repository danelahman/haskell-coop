{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for the file-runner in FileIO.
--

module FileIOTests where

import Control.Monad.Runner
import Control.Monad.Runner.FileIO

import System.IO hiding (withFile)

writeLines :: Member File iface => [String] -> User iface ()
writeLines [] = return ()
writeLines (l:ls) = do _ <- fWrite l;
                       fWrite "\n";
                       writeLines ls

exampleLines = ["Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
                "Cras sit amet felis arcu.",
                "Maecenas ac mollis mauris, vel fermentum nibh."]

test1 :: User '[IO] ()
test1 =                                   -- in IO signature, using IO container
  run
    fioRunner
    ioFioInitialiser
    (                                     -- in FileIO signature, using FIO runner
      run
        fhRunner
        (fioFhInitialiser "./out.txt")
        (                                 -- in File signature, using FH runner
          writeLines exampleLines
        )
        fioFhFinaliser
    )
    ioFioFinaliser

test2 = ioTopLevel test1

test3 :: User '[IO] ()
test3 =                                   -- in IO signature, using IO container
  run
    fioRunner
    ioFioInitialiser
    (                                     -- in FileIO signature, using FIO runner
      run
        fhRunner
        (fioFhInitialiser "./out2.txt")
        (                                 -- in File signature, using FH runner
          do s <- fRead;                  -- contents of the file at the point of entering the FH runner
             fWrite s;                    -- write the contents back to the file
             writeLines exampleLines      -- write additional lines to the file
        )
        fioFhFinaliser
    )
    ioFioFinaliser

test4 = ioTopLevel test3

test5 :: User '[IO] ()
test5 =                                            -- in IO signature, using IO container
  run
    fioRunner
    ioFioInitialiser
    (                                              -- in FileIO signature, using FIO runner
      run
        fhRunner
        (fioFhInitialiser "./out3.txt")
        (                                          -- in File signature, using FH runner
          do _ <- run
                    fcRunner
                    fhFcInitialiser
                    (                              -- in File signature, using FC runner
                      writeLines exampleLines      -- writing example lines using FC runner
                    )
                    fhFcFinaliser;
             fWrite "Proin eu porttitor enim."     -- writing another line using FH runner
        )
        fioFhFinaliser
    )
    ioFioFinaliser

test6 = ioTopLevel test5

test7 :: User '[IO] ()
test7 =                                            -- in IO signature, using IO container
  run
    fioRunner
    ioFioInitialiser
    (                                              -- in FileIO signature, using FIO runner
      run
        fhRunner
        (fioFhInitialiser "./out3.txt")
        (                                          -- in File signature, using FH runner
          do _ <- run
                    fcRunner
                    fhFcInitialiser
                    (                              -- in File signature, using FC runner
                      writeLines exampleLines      -- writing example lines using FC runner
                    )
                    (\ x s ->
                         do _ <- fWrite "Finalising FC runner\n";
                            fhFcFinaliser x s);
             fWrite "Proin eu porttitor enim.\n"     -- writing another line using FH runner
        )
        (\ x (s,fh) ->
             do _ <- fWriteOS fh "Finalising FH runner\n";
                fioFhFinaliser x (s,fh))
    )
    ioFioFinaliser

test8 = ioTopLevel test7

test9 :: User '[IO] ()
test9 =                                                     -- in IO signature, using IO container
  withFile
    "./out4.txt"
    (                                                       -- in File signature, using FIO,FH,FC runners
      do _ <- writeLines exampleLines;
         fWrite "\nProin eu porttitor enim.\n\n";
         s <- fRead;                                        -- the initial contents of the file
         fWrite "\nInitial contents of the file [START]\n";
         fWrite s;
         fWrite "\nInitial contents of the file [END]\n"
    )

test10 = ioTopLevel test9