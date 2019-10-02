{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Control.Monad.Comodel.OldFileIO where

import Control.Monad.Comodel
import System.IO

import qualified Data.ByteString.Char8 as B

--
-- Signatures for file-comodels with different capabilities.
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
-- File-fragment of the top-level IO-comodel, which
-- in turn is represented by the native IO monad.
--
fioCoOps :: FileIO r -> () -> Comp '[IO] (r,())
fioCoOps (OpenFile fn mode) _ =
  do fh <- focus (perform (openFile fn mode));
     return (fh,())
fioCoOps (CloseFile fh) _ =
  do _ <- focus (perform (hClose fh));
     return ((),())
fioCoOps (ReadFile fh) _ =
  do s <- focus (perform (B.hGetContents fh)); -- using ByteString IO to ensure strictness
     return (B.unpack s,())
fioCoOps (WriteFile fh s) _ =
  do _ <- focus (perform (B.hPutStr fh (B.pack s)));
     return ((),())

fioComodel :: Comodel '[IO] '[FileIO] ()
fioComodel = mkComodel fioCoOps

--
-- FH: File-comodel that operates on a single file (handle/name).
--
-- We have to store FilePath in the comodel because Handle does not persist
-- hGetContents. Thus the end result is not as elegant as one would wish.
--
fhCoOps :: File r -> FilePath -> Comp '[FileIO] (r,FilePath)
fhCoOps Read fn =
  do fh <- focus (perform (OpenFile fn ReadWriteMode));
     s <- focus (perform (ReadFile fh));
     focus (perform (CloseFile fh));
     return (s,fn)
fhCoOps (Write s) fn =
  do fh <- focus (perform (OpenFile fn AppendMode));
     focus (perform (WriteFile fh s));
     focus (perform (CloseFile fh));
     return ((),fn)

fhComodel :: Comodel '[FileIO] '[File] FilePath
fhComodel = mkComodel fhCoOps

--
-- FC: File-comodel that operates on the contents of a single file.
--
fcCoOps :: File r -> String -> Comp iface (r,String)
fcCoOps Read s = return (s , s)
fcCoOps (Write s') s = return (() , s ++ s')

fcComodel :: Comodel iface '[File] String
fcComodel = mkComodel fcCoOps

--
-- FC+OW: File-comodel that operates on the contents of a single file, but
-- which overwrites the existing contents of a file (in contrast with FC),
-- and it additionally supports on-demand emptying of the given file.
--
fcOwCoOps :: File r -> Either String String -> Comp iface (r,Either String String)
fcOwCoOps Read (Left s) = return (s , Left s)
fcOwCoOps Read (Right s) = return (s , Right s)
fcOwCoOps (Write s') (Left _) = return (() , Right s')
fcOwCoOps (Write s') (Right s) = return (() , Right (s ++ s'))

fcClCoOps :: Cleaner r -> Either String String -> Comp iface (r,Either String String)
fcClCoOps Clean _ = return (() , Right "")

fcOwComodel :: Comodel iface '[File,Cleaner] (Either String String)
fcOwComodel = unionComodels (mkComodel fcOwCoOps) (mkComodel fcClCoOps)

--
-- IO <-> FIO lens.
--
ioFioInitially :: Comp '[IO] ()
ioFioInitially = return ()

ioFioFinally :: () -> a -> Comp '[IO] a
ioFioFinally _ x = return x

ioFioLens :: IFLens '[IO] () a a
ioFioLens = mkIFLens ioFioInitially ioFioFinally

--
-- FIO <-> FH lens.
--
fioFhInitially :: FilePath -> Comp '[FileIO] FilePath
fioFhInitially fn = return fn

fioFhFinally :: FilePath -> a -> Comp '[FileIO] a
fioFhFinally _ x = return x

fioFhLens :: FilePath -> IFLens '[FileIO] FilePath a a
fioFhLens fn = mkIFLens (fioFhInitially fn) fioFhFinally

--
-- FH <-> FC lens.
--
fhFcInitially :: Comp '[File] String
fhFcInitially = focus (perform Read)

fhFcFinally :: String -> a -> Comp '[File] a
fhFcFinally s x = do _ <- focus (perform (Write s)); return x

fhFcLens :: IFLens '[File] String a a
fhFcLens = mkIFLens fhFcInitially fhFcFinally

--
-- FH <-> FC+OW lens.
--
fhFcOwInitially :: Comp '[File] (Either String String)
fhFcOwInitially = do s <- focus (perform Read); return (Left s)

fhFcOwFinally :: (Either String String) -> a -> Comp '[File] a
fhFcOwFinally (Left s) x = return x
fhFcOwFinally (Right s) x = do _ <- focus (perform (Write s)); return x

fhFcOwLens :: IFLens '[File] (Either String String) a a
fhFcOwLens = mkIFLens fhFcOwInitially fhFcOwFinally

--
-- FIO <-> FC lens.
--
fioFcInitially :: FilePath -> Comp '[FileIO] String
fioFcInitially fn =
  do fh <- focus (perform (OpenFile fn ReadWriteMode));
     s <- focus (perform (ReadFile fh));
     focus (perform (CloseFile fh));
     return s

fioFcFinally :: FilePath -> String -> a -> Comp '[FileIO] a
fioFcFinally fn s x =
  do fh <- focus (perform (OpenFile fn WriteMode));
     focus (perform (WriteFile fh s));
     focus (perform (CloseFile fh));
     return x

fioFcLens :: FilePath -> IFLens '[FileIO] String a a
fioFcLens fn = mkIFLens (fioFcInitially fn) (fioFcFinally fn)

--
-- FIO <-> FC+OW lens.
--
fioFcOwInitially :: FilePath -> Comp '[FileIO] (Either String String)
fioFcOwInitially fn =
  do fh <- focus (perform (OpenFile fn ReadWriteMode));
     s <- focus (perform (ReadFile fh));
     focus (perform (CloseFile fh));
     return (Left s)

fioFcOwFinally :: FilePath -> (Either String String) -> a -> Comp '[FileIO] a
fioFcOwFinally fn (Left s) x = return x
fioFcOwFinally fn (Right s) x =
  do fh <- focus (perform (OpenFile fn WriteMode));
     focus (perform (WriteFile fh s));
     focus (perform (CloseFile fh));
     return x

fioFcOwLens :: FilePath -> IFLens '[FileIO] (Either String String) a a
fioFcOwLens fn = mkIFLens (fioFcOwInitially fn) (fioFcOwFinally fn)