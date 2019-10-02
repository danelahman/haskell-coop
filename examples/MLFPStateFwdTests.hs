{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for running programs only on a footprint of the whole ML-style memory.
--
-- Here we use the variant of the footprint-based memory that internally only stores
-- the corresponding ML-style references and immediately delegates/forwards all get  
-- and put operations to the external MLState world (this is in contrast to the 
-- transactional variant of ML-style memory in MLFPState and MLFPStateTests).
--

module MLFPStateFwdTests where

import Control.Monad.Runner
import Control.Monad.Runner.MLState
import Control.Monad.Runner.MLFPStateFwd

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