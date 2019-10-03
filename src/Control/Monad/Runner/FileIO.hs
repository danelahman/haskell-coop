{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

--
-- Experiments with File IO using a variety of runners.
--

module Control.Monad.Runner.FileIO (
  FileIO, fOpenOS, fCloseOS, fReadOS, fWriteOS,
  File, fRead, fWrite,
  fioRunner, fhRunner, fcRunner,
  ioFioInitialiser, fioFhInitialiser, fhFcInitialiser,
  ioFioFinaliser, fioFhFinaliser, fhFcFinaliser,
  withFile
  ) where

import Control.Monad.Runner
import System.IO hiding (withFile)

import qualified Data.ByteString.Char8 as B

--
-- Signature of the file-fragment of the external world.
--
data FileIO :: * -> * where
  OpenFile  :: FilePath -> IOMode -> FileIO Handle
  CloseFile :: Handle -> FileIO ()
  ReadFile  :: Handle -> FileIO String
  WriteFile :: Handle -> String -> FileIO ()

--
-- Generic effects.
--
fOpenOS :: (Member FileIO iface) => FilePath -> IOMode -> User iface Handle
fOpenOS fn mode = focus (performU (OpenFile fn mode))

fCloseOS :: (Member FileIO iface) => Handle -> User iface ()
fCloseOS fh = focus (performU (CloseFile fh))

fReadOS :: (Member FileIO iface) => Handle -> User iface String
fReadOS fh = focus (performU (ReadFile fh))

fWriteOS :: (Member FileIO iface) => Handle -> String -> User iface ()
fWriteOS fh s = focus (performU (WriteFile fh s))

--
-- Signature for reading from and writing to files.
--
-- In the runners below, Read denotes reading the
-- initial value of a file when using a runner.
--
data File r where
  Read :: File String
  Write :: String -> File ()

--
-- Generic effects.
--
fRead :: (Member File iface) => User iface String
fRead = focus (performU Read)

fWrite :: (Member File iface) => String -> User iface ()
fWrite s = focus (performU (Write s))

--
-- FIO: File-fragment of the top-level IO-container.
--
-- The state of FIO is trivial because we cannot
-- internally access nor represent the real world.
--
type FIOState = ()

fioCoOps :: Member IO iface => FileIO a -> Kernel iface FIOState a
fioCoOps (OpenFile fn mode) =
  execK (focus (performU (openFile fn mode))) return
fioCoOps (CloseFile fh) =
  execK (focus (performU (hClose fh))) return
fioCoOps (ReadFile fh) =
  -- using ByteString IO to ensure strictness of IO
  execK (focus (performU (B.hGetContents fh))) (\ s -> return (B.unpack s))
fioCoOps (WriteFile fh s) =
  execK (focus (performU (B.hPutStr fh (B.pack s)))) return

fioRunner :: Member IO iface =>  Runner '[FileIO] iface FIOState
fioRunner = mkRunner fioCoOps

--
-- FH: File-runner that operates on a single file handle.
--
-- The state of FH is the initial contents of the file and
-- then a file handle supporting (over)writing to the file.
--
type FHState = (String , Handle)

fhCoOps :: Member FileIO iface => File a -> Kernel iface FHState a
fhCoOps Read =
  do (s,fh) <- getEnv;
     return s
fhCoOps (Write s') =
  do (s,fh) <- getEnv;
     execK (focus (performU (WriteFile fh s'))) return

fhRunner :: Member FileIO iface => Runner '[File] iface FHState
fhRunner = mkRunner fhCoOps

--
-- FC: File-runner that operates on the contents of a single file.
--
-- The state of FC is the initial contents of the file and
-- then the contents to be written to the file in finally.
--
type FCState = (String , String)

fcCoOps :: File a -> Kernel iface FCState a
fcCoOps Read =
  do (s,s') <- getEnv;
     return s
fcCoOps (Write s'') =
  do (s,s') <- getEnv;
     setEnv (s,s' ++ s'')

fcRunner :: Runner '[File] iface FCState
fcRunner = mkRunner fcCoOps

--
-- IO <-> FIO.
--
ioFioInitialiser :: Member IO iface => User iface FIOState
ioFioInitialiser = return ()

ioFioFinaliser :: Member IO iface => a -> FIOState -> User iface a
ioFioFinaliser x _ = return x

--
-- FIO <-> FH.
--
-- Initialiser: The contents of the file is read, and 
-- a file handle for writing to the file is opened.
--
-- Finaliser: The file handle is closed.
--
fioFhInitialiser :: Member FileIO iface => FilePath -> User iface FHState
fioFhInitialiser fn =
  do fh <- fOpenOS fn ReadWriteMode;
     s <- fReadOS fh;
     fCloseOS fh;
     fh <- fOpenOS fn WriteMode;
     return (s,fh)

fioFhFinaliser :: Member FileIO iface => a -> FHState -> User iface a
fioFhFinaliser x (_,fh) =
  do _ <- fCloseOS fh;
     return x
  
--
-- FH <-> FC.
--
-- Initialiser: The contents of the file is read, and 
-- the new contents to be written is initialised to "".
--
-- Finaliser: The new contents is written to the file.
--
fhFcInitialiser :: Member File iface => User iface FCState
fhFcInitialiser =
  do s <- fRead;
     return (s,"")

fhFcFinaliser :: Member File iface => a -> FCState -> User iface a
fhFcFinaliser x (_,s) =
  do _ <- fWrite s;
     return x

--
-- Derived with-file construct using the
-- composite IO <-> FIO <-> FH <-> FC.
--
withFile :: FilePath -> User '[File] a -> User '[IO] a
withFile fn m =
  run
    fioRunner
    ioFioInitialiser
    (
      run
        fhRunner
        (fioFhInitialiser fn)
        (
          run
            fcRunner
            fhFcInitialiser
            m
            fhFcFinaliser
        )
        fioFhFinaliser
    )
    ioFioFinaliser