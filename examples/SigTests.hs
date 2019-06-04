{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

--
-- Example tests for the signalling comodels in SigComodel.
--

module SigTests where

import Control.Monad.SigComodel

import System.IO

--
-- Handling a user-raised signal.
--
test1 :: Comp '[] Int
test1 =
  tryUnless (raise (-1)) (\ x -> return (24 + x)) (\ sig -> return (sig + 42))

test2 = runPure test1

--
-- Handling a signal-free program.
--
test3 :: Comp '[] Int
test3 =
  tryUnless (return 1) (\ x -> return (24 + x)) (\ sig -> return (sig + 42))

test4 = runPure test3

--
-- Resuming a program after dealing with a signal
-- by structuring the program appropriately.
--
data Ctr r where
  Lkp   :: Ctr Int
  Incr  :: Ctr ()
  Print :: Int -> Ctr ()

ctrCoOps :: Ctr r -> Int -> Comp '[IO] (r,Int)
ctrCoOps Lkp i       = return (i,i)
ctrCoOps Incr i      = return ((),i+1)
ctrCoOps (Print i) j =
  if (i == 42 || i == 84)
  then (raise (-1))
  else (do _ <- perform (print i); return ((),j))

ctrComodel :: Comodel '[IO] '[Ctr] Int
ctrComodel = mkComodel ctrCoOps

ctrLoop :: Comp '[Ctr] ()
ctrLoop =
  do i <- perform Lkp;
     perform Incr;
     perform (Print i);
     if i < 100 
     then ctrLoop
     else return ()

test5 :: Int -> Comp '[IO] ()
test5 i =
  run
    ctrComodel
    (mkIFLens (return i)
              (\ _ _ -> return ())
              (\ j _ -> do _ <- perform (print "signal received");
                           test5 j))
    ctrLoop

test6 = runIO (test5 0)

--
-- Emphasising control flow difference for user-raised signals
-- and signals raised by co-operations of the external world.
--
-- User-raised signals can be handled by inner tryUnless blocks.
-- Co-operation raised signals necessarily make the control flow
-- jump to the signal-handling clause of the finally block.
--
test7 :: Comp '[IO] Int
test7 =
  run
    ctrComodel
    (mkIFLens (return 0)
              (\ _ x -> return x)  -- (2) and then to here when handling a user-raised signal
              (\ _ _ -> return 42))
    (tryUnless (raise (-1))
               (\ _ -> return 24)
               (\ _ -> return 84)) -- (1) control first jumps here when handling a user-raised signal

test8 = runIO test7

test9 :: Comp '[IO] Int
test9 =
  run
    ctrComodel
    (mkIFLens (return 0)
              (\ _ x -> return x)
              (\ _ _ -> return 42)) -- control jumps here when handling a co-operation signal
    (tryUnless (perform (Print 42))
               (\ _ -> return 24)
               (\ _ -> return 84))

test10 = runIO test9

--
-- Resuming a program after dealing with a signal
-- by using a suitable (trace-saving) comodel.
--
-- The important part in making that happen is that
-- the external world supports a "replay" mode.
--
data StringIO r where
  SRead  :: StringIO String
  SPrint :: String -> StringIO ()

strCoOps :: StringIO r -> () -> Comp '[ReplayIO] (r,())
strCoOps SRead _ =
  do b <- perform MCheckRMode;
     s <- perform MRead;
     if b
     then return (s,())         -- don't (re-)raise signals when replaying
     else
       (
         if s == "signal"
         then raise (-1)        -- entering "signal" leads to program being resumed by replaying
         else
           (
             if s == "quit"
             then raise 0       -- entering "quit" will lead to the program being terminated
             else return (s,())
           )
       )
strCoOps (SPrint s) _ =
  do _ <- perform (MPrint s);
     return ((),())

strComodel :: Comodel '[ReplayIO] '[StringIO] ()
strComodel = mkComodel strCoOps

data ReplayIO r where
  MRead       :: ReplayIO String
  MPrint      :: String -> ReplayIO ()
  MSetRMode   :: ReplayIO ()         -- switching on the "replay" mode
  MCheckRMode :: ReplayIO Bool       -- checking whether the "replay" mode is on

type Replay = [Either String ()]
type Trace = [Either String ()]

replayCoOps :: ReplayIO r -> (Replay,Trace) -> Comp '[IO] (r,(Replay,Trace))

replayCoOps MRead ([],trace) =
  do s <- perform getLine;
     return (s,([],trace ++ [Left s]))
replayCoOps MRead (Left s : replay,trace) =
  do _ <- perform (print ("DEBUG: Replayed MRead with '" ++ s ++ "'"));
     return (s,(replay,trace ++ [Left s]))
replayCoOps MRead (Right _ : replay,trace) =
  error "incompatible state for replaying the program"
  
replayCoOps (MPrint s) ([],trace) =
  do _ <- perform (print s);
     return ((),([],trace ++ [Right ()]))
replayCoOps (MPrint _) (Left s : replay,trace) =
  error "incompatible state for replaying the program"
replayCoOps (MPrint s) (Right _ : replay,trace) =
  return ((),(replay,trace ++ [Right ()]))

replayCoOps MSetRMode ([],trace) =
  return ((),(trace,[]))
replayCoOps MSetRMode (_,trace) =
  error "incompatible state for switching the replay mode on"

replayCoOps MCheckRMode (replay,trace) =
  return (length replay > 0,(replay,trace))

replayComodel :: Comodel '[IO] '[ReplayIO] (Replay,Trace)
replayComodel = mkComodel replayCoOps

ioLoop :: Comp '[StringIO] ()  -- a program that knows nothing about replaying
ioLoop =
  do s <- perform SRead;
     perform (SPrint ("You inputted '" ++ s ++ "'"));
     ioLoop

test11 :: Comp '[ReplayIO] Int
test11 =
  run
    strComodel
    (mkIFLens (return ())
              (\ _ _ -> return (-42))
              (\ _ sig ->
                 if sig == 0
                 then return 42
                 else ({- doSomethingInterestingBeforeReplaying; -}
                       do _ <- perform MSetRMode;
                          test11)))
    ioLoop

test12 :: Comp '[IO] Int
test12 =
  run
    replayComodel
    (mkIFLens (return ([],[]))
              (\ _ x -> return x)
              (\ _ sig -> raise sig))
    (
      test11
    )

test13 = runIO test12