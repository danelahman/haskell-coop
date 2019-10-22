{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Control.Runner.OldFileIO
Description : Runners implementing file IO
Copyright   : (c) Danel Ahman, 2019
License     : MIT
Maintainer  : danel.ahman@eesti.ee
Stability   : experimental

This module provides a variety of runners implementing file IO. 
These runners mostly differ in what they store in their runtime 
state, e.g., storing a file handle vs storing the accumulated 
writes to a file. 

It differs from what can be found in the (newer) Control.Runner.FileIO 
module in that the file handle `fhRunner` does not store the file handle 
in its runtime state but instead the file name. This is to accommodate 
the non-persistence of Haskell file handles across calls to `hGetContents`.
-}
module Control.Runner.OldFileIO
  (
  FileIO(..), File(..), Cleaner(..),
  fioRunner, fhRunner, fcRunner, fcOwRunner, 
  ioFioInitialiser, ioFioFinaliser,
  fioFhInitialiser, fioFhFinaliser,
  fhFcInitialiser, fhFcFinaliser,
  fhFcOwInitialiser, fhFcOwFinaliser,
  fioFcInitialiser, fioFcFinaliser,
  fioFcOwInitialiser, fioFcOwFinaliser
  ) where

import Control.Runner
import System.IO

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

-- | An effect for performing reads and writes (on a file whose file
-- handle is hidden by the user code through the use of runners).
data File a where
  -- | Algebraic operation for reading (from a file that is hidden from user code).
  Read  :: File String
  -- | Algebraic operation for writing (to a file that is hidden from user code).
  Write :: String -> File ()

-- | An effect to empty a given file.
data Cleaner a where
  -- | Algebraic operation that empties the contents of a given file.
  Clean :: Cleaner ()

-- | The co-operations of the `fioRunner`.
fioCoOps :: FileIO a -> Kernel '[IO] () a
fioCoOps (OpenFile fn mode) =
  user (focus (performU (openFile fn mode))) return
fioCoOps (CloseFile fh) =
  user (focus (performU (hClose fh))) return
fioCoOps (ReadFile fh) =
  user (focus (performU (B.hGetContents fh))) (\ s -> return (B.unpack s))
fioCoOps (WriteFile fh s) =
  user (focus (performU (B.hPutStr fh (B.pack s)))) return

-- | Runner that implements the `FileIO` effect, by delegating
-- the file IO operations to Haskell's `IO` monad operations.
--
-- Intuitively, this runner focusses on a fraction of the larger,
-- external signature (namely, that of the `IO` monad).
--
-- Its runtime state is trivial because this runner directly delegates
-- the file IO operations to Haskell's `IO` monad operations.
fioRunner :: Runner '[FileIO] '[IO] ()
fioRunner = mkRunner fioCoOps

-- | The co-operations of the `fhRunner`.
fhCoOps :: File a -> Kernel '[FileIO] FilePath a
fhCoOps Read =
  do fn <- getEnv;
     fh <- performK (OpenFile fn ReadWriteMode);
     s <- performK (ReadFile fh);
     performK (CloseFile fh);
     return s
fhCoOps (Write s) =
  do fn <- getEnv;
     fh <- performK (OpenFile fn AppendMode);
     performK (WriteFile fh s);
     performK (CloseFile fh)

-- | Runner that implements the `File` effect, by
-- internally storing a given file name (of type `FilePath`),
-- and implementing the co-operations for the `File`
-- effect using the operations of the `FileIO` effect.
--
-- Both co-operations follow a similar pattern: open the
-- file, do the `File` effect, and then close the file,
-- while keeping the stored file name unchanged.
fhRunner :: Runner '[File] '[FileIO] FilePath
fhRunner = mkRunner fhCoOps

-- | The co-operations of the `fcRunner`.
fcCoOps :: File a -> Kernel sig String a
fcCoOps Read =
  getEnv
fcCoOps (Write s') =
  do s <- getEnv;
     setEnv (s ++ s')

-- | Runner that implements the `File` effect,
-- by returning the internally stored contents
-- on `Read` operations, and accumulates any 
-- `Write` operations in its runtime state.
fcRunner :: Runner '[File] sig String
fcRunner = mkRunner fcCoOps

--
-- FC+OW: File-runner that operates on the contents of a single file, but
-- which overwrites the existing contents of a file (in contrast with FC),
-- and it additionally supports on-demand emptying of the given file.
--

-- | The co-operations of the `fcOwRunner` runner.
fcOwCoOpsAux :: File a -> Either String String -> Kernel sig (Either String String) a
fcOwCoOpsAux Read (Left s) =
  return s
fcOwCoOpsAux Read (Right s) =
  return s
fcOwCoOpsAux (Write s') (Left _) =
  setEnv (Right s')
fcOwCoOpsAux (Write s') (Right s) =
  setEnv (Right (s ++ s'))

-- | The co-operations of the `fcOwRunner` runner.
fcOwCoOps :: File a -> Kernel sig (Either String String) a
fcOwCoOps f =
  do s <- getEnv;
     fcOwCoOpsAux f s

-- | The co-operations of the `fcOwRunner` runner.
fcClCoOps :: Cleaner a -> Kernel sig (Either String String) a
fcClCoOps Clean = setEnv (Right "")

-- | Runner that implements the union of the `File` and `Cleaner`
-- effects, by operating on the contents of a single file,
-- overwriting the existing contents of a file (in contrast to
-- `fcRunner`), and additionally allowing for on-demand emptying
-- of the file (using the `Clean` co-operation).
--
-- The runtime state of this runner is either the initial contents
-- of the file (modelled as @Left s@), or the contents overwriting
-- the initial one (modelled as @Right s@).
--
-- The `Write` co-operation overwrites the initial contents, and
-- then starts accumulating the subsequent writes. The `Read`
-- co-operation returns whatever string is currently stored in
-- the runtime state (be it initial or new). The `Clean` co-operation
-- simply sets the runtime state to @Right ""@, i.e., empties the file.
fcOwRunner :: Runner '[File,Cleaner] sig (Either String String)
fcOwRunner = unionRunners (mkRunner fcOwCoOps) (mkRunner fcClCoOps)

-- | Initialiser for the runner `fioRunner`
-- in the `IO` monad external context. 
ioFioInitialiser :: User '[IO] ()
ioFioInitialiser = return ()

-- | Finaliser for the runner `fioRunner`
-- in the `IO` monad external context.
--
-- As the runtime state of the `fioRunner` is trivial,
-- the finaliser simply passes on the return value.
ioFioFinaliser :: a -> () -> User '[IO] a
ioFioFinaliser x _ = return x

-- | Initialiser for the runner `fhRunner`,
-- in the `FileIO` effect external context.
--
-- It simply returns the given file path.
fioFhInitialiser :: FilePath -> User '[FileIO] FilePath
fioFhInitialiser fn = return fn

-- | Finaliser for the runner `fhRunner`,
-- in the `FileIO` effect external context.
fioFhFinaliser :: a -> FilePath  -> User '[FileIO] a
fioFhFinaliser x _ = return x

-- | Initialiser for the runner `fcRunner`,
-- in the `File` effect external context.
--
-- It reads (with `Read`) and returns the initial
-- contents of the given file.
fhFcInitialiser :: User '[File] String
fhFcInitialiser =
  performU Read

-- | Finaliser for the runner `fcRunner`,
-- in the `File` effect external context.
--
-- It writes the accumulated writes with `Write`
-- and passes on the return value.
fhFcFinaliser :: a -> String -> User '[File] a
fhFcFinaliser x s =
  do performU (Write s);
     return x

-- | Initialiser for the runner `fcOwRunner`,
-- in the `File` effect external context.
--
-- It reads and returns the initial contents of the given file.
fhFcOwInitialiser :: User '[File] (Either String String)
fhFcOwInitialiser =
  do s <- performU Read;
     return (Left s)

-- | Finaliser for the runner `fcOwRunner`,
-- in the `File` effect external context.
--
-- If the final value of the runtime state is still
-- the initial contents of the file (it is @Left s@),
-- then it simply passes on the return value. If the
-- original contents of the file has been overwritten,
-- then the finaliser writes it with `Write`, and then
-- passes on the return value.
fhFcOwFinaliser :: a -> (Either String String) -> User '[File] a
fhFcOwFinaliser x (Left s) =
  return x
fhFcOwFinaliser x (Right s) =
  do performU (Write s);
     return x

-- | Initialiser for the runner `fcRunner`,
-- in the `FileIO` effect external context.
--
-- It opens the given file in reading mode, reads
-- the contents, closes the file, and returns the contents.
fioFcInitialiser :: FilePath -> User '[FileIO] String
fioFcInitialiser fn =
  do fh <- performU (OpenFile fn ReadWriteMode);
     s <- performU (ReadFile fh);
     performU (CloseFile fh);
     return s

-- | Finaliser for the runner `fcRunner`,
-- in the `FileIO` effect external context.
--
-- It opens the given file in (over-)writing mode, writes
-- the final value of the runtime state to the file,
-- closes the file, and passes on the return value.
fioFcFinaliser :: FilePath -> a -> String -> User '[FileIO] a
fioFcFinaliser fn x s =
  do fh <- performU (OpenFile fn WriteMode);
     performU (WriteFile fh s);
     performU (CloseFile fh);
     return x

-- | Initialiser for the runner `fcOwRunner`,
-- in the `FileIO` effect external context.
--
-- It opens the given file in reading mode, reads
-- the contents, closes the file, and returns the
-- contents (as a left inject in @Either String String@).
fioFcOwInitialiser :: FilePath -> User '[FileIO] (Either String String)
fioFcOwInitialiser fn =
  do fh <- performU (OpenFile fn ReadWriteMode);
     s <- performU (ReadFile fh);
     performU (CloseFile fh);
     return (Left s)

-- | Finaliser for the runner `fcOwRunner`,
-- in the `FileIO` effect external context.
--
-- If the final value of the runtime state is still
-- equal to the initial contents of the file, it
-- simply passes on the return value. If the contents
-- of the file has changed, it opens the file in
-- (over-)writing mode, writes the final value of the
-- runtime state to the file, closes the file, and
-- passes on the return value.
fioFcOwFinaliser :: FilePath -> a -> (Either String String) -> User '[FileIO] a
fioFcOwFinaliser fn x (Left s) = return x
fioFcOwFinaliser fn x (Right s) =
  do fh <- performU (OpenFile fn WriteMode);
     performU (WriteFile fh s);
     performU (CloseFile fh);
     return x