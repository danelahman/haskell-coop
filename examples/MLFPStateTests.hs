{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for running programs only on a footprint of the whole ML-style memory.
--

module MLFPStateTests where

import Control.Monad.Runner
import Control.Monad.Runner.FPState
import Control.Monad.Runner.MLState
import Control.Monad.Runner.MLFPState

test1 :: User '[MLState] (String,String,String,Bool)
test1 =
  do r <- alloc "foobar";
     r' <- alloc "foo";
     r'' <- alloc "bar";
     r''' <- alloc True;
     withFootprint (FC r' (FC r'' FE)) (
         do s <- get AZ;
            s' <- get (AS AZ);
            put AZ s';
            put (AS AZ) s
       );
     s <- (!) r;
     s' <- (!) r';
     s'' <- (!) r'';
     x <- (!) r''';
     return (s,s',s'',x)

test2 = mlTopLevel test1 -- expected result ("foobar","bar","foo",True)