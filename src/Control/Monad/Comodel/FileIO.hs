{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Control.Monad.Comodel.FileIO (
  FileIO, openF, closeF, readF, writeF,
  File, fRead, fWrite,
  fioComodel, fhComodel, fcComodel,
  ioFioLens, fioFhLens, fhFcLens,
  withFile
  ) where

import Control.Monad.Comodel
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
-- Human-readable syntax for FileIO operations.
--
openF :: (Member FileIO iface) => FilePath -> IOMode -> Comp iface Handle
openF fn mode = focus (perform (OpenFile fn mode))

closeF :: (Member FileIO iface) => Handle -> Comp iface ()
closeF fh = focus (perform (CloseFile fh))

readF :: (Member FileIO iface) => Handle -> Comp iface String
readF fh = focus (perform (ReadFile fh))

writeF :: (Member FileIO iface) => Handle -> String -> Comp iface ()
writeF fh s = focus (perform (WriteFile fh s))

--
-- Signature for reading from and writing to files.
--
-- In the comodels below, Read denotes reading the
-- initial value of a file when using a comodel.
--
data File r where
  Read :: File String
  Write :: String -> File ()

--
-- Human-readable syntax for File operations.
--
fRead :: (Member File iface) => Comp iface String
fRead = focus (perform Read)

fWrite :: (Member File iface) => String -> Comp iface ()
fWrite s = focus (perform (Write s))

--
-- FIO: File-fragment of the top-level IO-comodel.
--
-- The state of FIO is trivial because we cannot
-- internally access nor represent the real world.
--
type FIOState = ()

fioCoOps :: Member IO iface => FileIO r -> FIOState -> Comp iface (r,FIOState)
fioCoOps (OpenFile fn mode) _ =
  do fh <- focus (perform (openFile fn mode));
     return (fh,())
fioCoOps (CloseFile fh) _ =
  do _ <- focus (perform (hClose fh));
     return ((),())
fioCoOps (ReadFile fh) _ =
  -- using ByteString IO to ensure strictness of IO
  do s <- focus (perform (B.hGetContents fh));
     return (B.unpack s,())
fioCoOps (WriteFile fh s) _ =
  do _ <- focus (perform (B.hPutStr fh (B.pack s)));
     return ((),())

fioComodel :: Member IO iface =>  Comodel iface '[FileIO] FIOState
fioComodel = mkComodel fioCoOps

--
-- FH: File-comodel that operates on a single file handle.
--
-- The state of FH is the initial contents of the file and
-- then a file handle supporting (over)writing to the file.
--
type FHState = (String , Handle)

fhCoOps :: Member FileIO iface => File r -> FHState -> Comp iface (r,FHState)
fhCoOps Read (s,fh) = return (s,(s,fh))
fhCoOps (Write s') (s,fh) =
  do _ <- focus (perform (WriteFile fh s')); return ((),(s,fh))

fhComodel :: Member FileIO iface => Comodel iface '[File] FHState
fhComodel = mkComodel fhCoOps

--
-- FC: File-comodel that operates on the contents of a single file.
--
-- The state of FC is the initial contents of the file and
-- then the contents to be written to the file in finally.
--
type FCState = (String , String)

fcCoOps :: File r -> FCState -> Comp iface (r,FCState)
fcCoOps Read (s,s') = return (s,(s,s'))
fcCoOps (Write s'') (s,s') = return ((),(s,s' ++ s''))

fcComodel :: Comodel iface '[File] FCState
fcComodel = mkComodel fcCoOps

--
-- IO <-> FIO lens.
--
-- Initially & finall: Have trivial effect on the comodel.
--
ioFioInitially :: Member IO iface => Comp iface FIOState
ioFioInitially = return ()

ioFioFinally :: Member IO iface => FIOState -> a -> Comp iface a
ioFioFinally _ x = return x

ioFioLens :: Member IO iface => IFLens iface FIOState a a
ioFioLens = mkIFLens ioFioInitially ioFioFinally

--
-- FIO <-> FH lens.
--
-- Initially: The contents of the file is read, and 
-- a file handle for writing to the file is opened.
--
-- Finally: The file handle is closed.
--
fioFhInitially :: Member FileIO iface => FilePath -> Comp iface FHState
fioFhInitially fn =
  do fh <- focus (perform (OpenFile fn ReadWriteMode));
     s <- focus (perform (ReadFile fh));
     focus (perform (CloseFile fh));
     fh <- focus (perform (OpenFile fn WriteMode));
     return (s,fh)

fioFhFinally :: Member FileIO iface => FHState -> a -> Comp iface a
fioFhFinally (_,fh) x =
  do _ <- focus (perform (CloseFile fh)); return x

fioFhLens :: Member FileIO iface => FilePath -> IFLens iface FHState a a
fioFhLens fn = mkIFLens (fioFhInitially fn) fioFhFinally

--
-- FH <-> FC lens.
--
-- Initially: The contents of the file is read, and 
-- the new contents to be written is initialised to "".
--
-- Finally: The new contents is written to the file.
--
fhFcInitially :: Member File iface => Comp iface FCState
fhFcInitially = do s <- focus (perform Read); return (s,"")

fhFcFinally :: Member File iface => FCState -> a -> Comp iface a
fhFcFinally (_,s) x = do _ <- focus (perform (Write s)); return x

fhFcLens :: Member File iface => IFLens iface FCState a a
fhFcLens = mkIFLens fhFcInitially fhFcFinally

--
-- Derived using-file construct using the
-- composite IO <-> FIO <-> FH <-> FC lens.
--
withFile :: FilePath -> Comp '[File] a -> Comp '[IO] a
withFile fn c =
  run
    fioComodel
    ioFioLens
    (
      run
        fhComodel
        (fioFhLens fn)
        (
          run
            fcComodel
            fhFcLens
            c
        )
    )