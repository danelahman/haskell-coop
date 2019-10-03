{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

--
-- Experiments with File IO using a variety of runners.
--

module Control.Monad.Runner.OldFileIO where

import Control.Monad.Runner
import System.IO

import qualified Data.ByteString.Char8 as B

--
-- Signatures for file-runners with different capabilities.
--
data FileIO :: * -> * where
  OpenFile  :: FilePath -> IOMode -> FileIO Handle
  CloseFile :: Handle -> FileIO ()
  ReadFile  :: Handle -> FileIO String
  WriteFile :: Handle -> String -> FileIO ()

data File :: * -> * where
  Read  :: File String
  Write :: String -> File ()

data Cleaner :: * -> * where
  Clean :: Cleaner ()

--
-- File-fragment of the top-level IO-container, which
-- in turn is represented by the native IO monad.
--
fioCoOps :: FileIO a -> Kernel '[IO] () a
fioCoOps (OpenFile fn mode) =
  execK (focus (performU (openFile fn mode))) return
fioCoOps (CloseFile fh) =
  execK (focus (performU (hClose fh))) return
fioCoOps (ReadFile fh) =
  execK (focus (performU (B.hGetContents fh))) (\ s -> return (B.unpack s))
fioCoOps (WriteFile fh s) =
  execK (focus (performU (B.hPutStr fh (B.pack s)))) return

fioRunner :: Runner '[FileIO] '[IO] ()
fioRunner = mkRunner fioCoOps

--
-- FH: File-runner that operates on a single file (handle/name).
--
-- We have to store FilePath in the runner because Handle does not persist
-- hGetContents. Thus the end result is not as elegant as one would wish.
--
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

fhRunner :: Runner '[File] '[FileIO] FilePath
fhRunner = mkRunner fhCoOps

--
-- FC: File-runner that operates on the contents of a single file.
--
fcCoOps :: File a -> Kernel sig String a
fcCoOps Read =
  getEnv
fcCoOps (Write s') =
  do s <- getEnv;
     setEnv (s ++ s')

fcRunner :: Runner '[File] sig String
fcRunner = mkRunner fcCoOps

--
-- FC+OW: File-runner that operates on the contents of a single file, but
-- which overwrites the existing contents of a file (in contrast with FC),
-- and it additionally supports on-demand emptying of the given file.
--
fcOwCoOpsAux :: File a -> Either String String -> Kernel sig (Either String String) a
fcOwCoOpsAux Read (Left s) =
  return s
fcOwCoOpsAux Read (Right s) =
  return s
fcOwCoOpsAux (Write s') (Left _) =
  setEnv (Right s')
fcOwCoOpsAux (Write s') (Right s) =
  setEnv (Right (s ++ s'))

fcOwCoOps :: File a -> Kernel sig (Either String String) a
fcOwCoOps f =
  do s <- getEnv;
     fcOwCoOpsAux f s

fcClCoOps :: Cleaner a -> Kernel sig (Either String String) a
fcClCoOps Clean = setEnv (Right "")

fcOwRunner :: Runner '[File,Cleaner] sig (Either String String)
fcOwRunner = unionRunners (mkRunner fcOwCoOps) (mkRunner fcClCoOps)

--
-- IO <-> FIO.
--
ioFioInitialiser :: User '[IO] ()
ioFioInitialiser = return ()

ioFioFinaliser :: a -> () -> User '[IO] a
ioFioFinaliser x _ = return x

--
-- FIO <-> FH.
--
fioFhInitialiser :: FilePath -> User '[FileIO] FilePath
fioFhInitialiser fn = return fn

fioFhFinaliser :: a -> FilePath  -> User '[FileIO] a
fioFhFinaliser x _ = return x

--
-- FH <-> FC.
--
fhFcInitialiser :: User '[File] String
fhFcInitialiser =
  performU Read

fhFcFinaliser :: a -> String -> User '[File] a
fhFcFinaliser x s =
  do performU (Write s);
     return x

--
-- FH <-> FC+OW.
--
fhFcOwInitialiser :: User '[File] (Either String String)
fhFcOwInitialiser =
  do s <- performU Read;
     return (Left s)

fhFcOwFinaliser :: a -> (Either String String) -> User '[File] a
fhFcOwFinaliser x (Left s) =
  return x
fhFcOwFinaliser x (Right s) =
  do performU (Write s);
     return x

--
-- FIO <-> FC.
--
fioFcInitialiser :: FilePath -> User '[FileIO] String
fioFcInitialiser fn =
  do fh <- performU (OpenFile fn ReadWriteMode);
     s <- performU (ReadFile fh);
     performU (CloseFile fh);
     return s

fioFcFinaliser :: FilePath -> a -> String -> User '[FileIO] a
fioFcFinaliser fn x s =
  do fh <- performU (OpenFile fn WriteMode);
     performU (WriteFile fh s);
     performU (CloseFile fh);
     return x

--
-- FIO <-> FC+OW.
--
fioFcOwInitialiser :: FilePath -> User '[FileIO] (Either String String)
fioFcOwInitialiser fn =
  do fh <- performU (OpenFile fn ReadWriteMode);
     s <- performU (ReadFile fh);
     performU (CloseFile fh);
     return (Left s)

fioFcOwFinaliser :: FilePath -> a -> (Either String String) -> User '[FileIO] a
fioFcOwFinaliser fn x (Left s) = return x
fioFcOwFinaliser fn x (Right s) =
  do fh <- performU (OpenFile fn WriteMode);
     performU (WriteFile fh s);
     performU (CloseFile fh);
     return x