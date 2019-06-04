{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for running programs only on a footprint of the whole ML-style memory.
--

module MLFPStateTests where

import Control.Monad.Comodel
import Control.Monad.Comodel.FPState
import Control.Monad.Comodel.MLState
import Control.Monad.Comodel.MLFPState

test1 :: Comp '[MLState] (String,String,String,Bool)
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

test2 = topLevel test1