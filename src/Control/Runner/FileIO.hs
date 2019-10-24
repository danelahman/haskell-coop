{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Control.Runner.FileIO
Description : Runners implementing file IO
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides a variety of runners implementing file IO. 
These runners mostly differ in what they store in their runtime 
state, e.g., storing a file handle vs storing the accumulated 
writes to a file. 
-}
module Control.Runner.FileIO (
  FileIO(..), fOpenOS, fCloseOS, fReadOS, fWriteOS,
  File(..), fRead, fWrite, FIOState, FHState, FCState, 
  fioRunner, fhRunner, fcRunner,
  ioFioInitialiser, fioFhInitialiser, fhFcInitialiser,
  ioFioFinaliser, fioFhFinaliser, fhFcFinaliser,
  withFile
  ) where

import Control.Runner
import System.IO hiding (withFile)

import qualified Data.ByteString.Char8 as B

-- | An effect for performing file IO.
data FileIO a where
  -- | Algebraic operation for opening a file in a given mode.
  OpenFile  :: FilePath -> IOMode -> FileIO Handle
  -- | Algebraic operation of closing a given file.
  CloseFile :: Handle -> FileIO ()
  -- | Algebraic operation for reading from a given file.
  ReadFile  :: Handle -> FileIO String
  -- | Algebraic operation for writing to a given file.
  WriteFile :: Handle -> String -> FileIO ()

-- | Generic effect for opening a file in a given mode.
fOpenOS :: (Member FileIO sig) => FilePath -> IOMode -> User sig Handle
fOpenOS fn mode = performU (OpenFile fn mode)

-- | Generic effect for closing a given file.
fCloseOS :: (Member FileIO sig) => Handle -> User sig ()
fCloseOS fh = performU (CloseFile fh)

-- | Generic effect for reading from a given file.
fReadOS :: (Member FileIO sig) => Handle -> User sig String
fReadOS fh = performU (ReadFile fh)

-- | Generic effect for writing to a given file.
fWriteOS :: (Member FileIO sig) => Handle -> String -> User sig ()
fWriteOS fh s = performU (WriteFile fh s)

-- | An effect for performing reads and writes (on a file whose file
-- handle is hidden by the user code through the use of runners).
--
-- In this module, we additionally suppose that Read denotes 
-- reading the initial value of a file when using a runner.
data File a where
  -- | Algebraic operation for reading (from a file that is hidden from user code).
  Read :: File String
  -- | Algebraic operation for writing (to a file that is hidden from user code).
  Write :: String -> File ()

-- | Generic effect for reading (from a file that is hidden from user code).
fRead :: (Member File sig) => User sig String
fRead = performU Read

-- | Generic effect for writing (to a file that is hidden from user code).
fWrite :: (Member File sig) => String -> User sig ()
fWrite s = performU (Write s)

--
-- FIO: File-fragment of the top-level IO-container.
--
-- The state of FIO is trivial because we cannot
-- internally access nor represent the real world.
--

-- | Type of the runtime state of the runner `fioRunner`.
-- 
-- The state is trivial because this runner directly delegates
-- the file IO operations to Haskell's `IO` monad operations.
type FIOState = ()

-- | The co-operations of the runner `fioRunner`.
fioCoOps :: Member IO sig => FileIO a -> Kernel sig FIOState a
fioCoOps (OpenFile fn mode) =
  performK (openFile fn mode)
fioCoOps (CloseFile fh) =
  performK (hClose fh)
fioCoOps (ReadFile fh) =
  -- using ByteString IO to ensure strictness of IO
  do s <- performK (B.hGetContents fh);
     return (B.unpack s)
fioCoOps (WriteFile fh s) =
  performK (B.hPutStr fh (B.pack s))

-- | Runner that implements the `FileIO` effect, by delegating
-- the file IO operations to Haskell's `IO` monad operations.
--
-- Intuitively, this runner focusses on a fraction of the larger,
-- external signature (namely, that of the `IO` monad).
fioRunner :: Member IO sig =>  Runner '[FileIO] sig FIOState
fioRunner = mkRunner fioCoOps

-- | Type of the runtime state of the runner `fhRunner`.
--
-- The state comprises the initial contents of the file
-- and then a file handle supporting (over)writing to the file.
type FHState = (String , Handle)

-- | Type co-operations of the runner `fhRunner`.
fhCoOps :: Member FileIO sig => File a -> Kernel sig FHState a
fhCoOps Read =
  do (s,fh) <- getEnv;
     return s
fhCoOps (Write s') =
  do (s,fh) <- getEnv;
     performK (WriteFile fh s')

-- | Runner that implements the `File` effect, by
-- returning the internally stored (initial) contents
-- on `Read` operations, and delegates `Write` operations
-- to some enveloping runner for the `FileIO` effect,
-- using the file handle stored in its runtime state.
fhRunner :: Member FileIO sig => Runner '[File] sig FHState
fhRunner = mkRunner fhCoOps

--
-- FC: File-runner that operates on the contents of a single file.
--
-- The state of FC is the initial contents of the file and
-- then the contents to be written to the file in finally.
--

-- | Type of the runtime state of the runner `fcRunner`.
--
-- The state comprises the initial contents of the file,
-- and then an accumulator for strings to be written to
-- the file in the finalisation (when running with `fcRunner`).
type FCState = (String , String)

-- | The co-operations of the runner `fcRunner`.
fcCoOps :: File a -> Kernel sig FCState a
fcCoOps Read =
  do (s,s') <- getEnv;
     return s
fcCoOps (Write s'') =
  do (s,s') <- getEnv;
     setEnv (s,s' ++ s'')

-- | Runner that implements the `File` effect,
-- by returning the internally stored (initial)
-- contents on `Read` operations, and accumulates
-- any `Write` operations in its runtime state.
fcRunner :: Runner '[File] sig FCState
fcRunner = mkRunner fcCoOps

--
-- IO <-> FIO.
--

-- | Initialiser for the runner `fioRunner`
-- in the `IO` monad external context. 
ioFioInitialiser :: Member IO sig => User sig FIOState
ioFioInitialiser = return ()

-- | Finaliser for the runner `fioRunner`
-- in the `IO` monad external context.
--
-- As the runtime state of the `fioRunner` is trivial,
-- the finaliser simply passes on the return value.
ioFioFinaliser :: Member IO sig => a -> FIOState -> User sig a
ioFioFinaliser x _ = return x

-- | Initialiser for the runner `fhRunner`,
-- in the `FileIO` effect external context.
--
-- It first reads the initial contents of the given 
-- file and then it opens the file for writing,
-- returning the initial contents and the file handle.
fioFhInitialiser :: Member FileIO sig => FilePath -> User sig FHState
fioFhInitialiser fn =
  do fh <- fOpenOS fn ReadWriteMode;
     s <- fReadOS fh;
     fCloseOS fh;
     fh <- fOpenOS fn WriteMode;
     return (s,fh)

-- | Finaliser for the runner `fhRunner`,
-- in the `FileIO` effect external context.
--
-- It closes the file given file handle, and passes
-- on the return value.
fioFhFinaliser :: Member FileIO sig => a -> FHState -> User sig a
fioFhFinaliser x (_,fh) =
  do fCloseOS fh;
     return x
  
-- | Initialiser for the runner `fcRunner`,
-- in the `File` effect external context.
--
-- It first reads the initial contents of the given 
-- file, and then returns the contents and the empty
-- accumulator for `Write` operations.
fhFcInitialiser :: Member File sig => User sig FCState
fhFcInitialiser =
  do s <- fRead;
     return (s,"")

-- | Finaliser for the runner `fcRunner`,
-- in the `File` effect external context.
--
-- It writes the accumulated writes with `Write`
-- and passes on the return value.
fhFcFinaliser :: Member File sig => a -> FCState -> User sig a
fhFcFinaliser x (_,s) =
  do fWrite s;
     return x

--
-- Derived with-file construct using the
-- composite IO <-> FIO <-> FH <-> FC.
--

-- | Derived with-file construct that runs user code with
-- the `File` effect in the external context of the `IO`
-- monad.
--
-- This construct nests all the different runners implemented
-- in this module, as follows
--
-- > IO monad <-> fioRunner <-> fhRunner <-> fcRunner <-> user code
--
-- with the arrows informally denoting the various initialisers
-- and finalisers for the different runners.
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