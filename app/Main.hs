{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet

import System.IO (hPutStrLn, stderr)
import Control.Monad (forever)
import Data.Binary
import Data.Typeable
import GHC.Generics

import Control.Monad.Free

data Msg = Msg String deriving (Eq, Show, Generic, Typeable)

instance Binary Msg


listenAndPrint :: () -> Process ()
listenAndPrint _ = forever $ do
  receiveWait [match processMessage]
  where
    processMessage :: Msg -> Process ()
    processMessage (Msg msg) = do
      liftIO $ putStrLn msg


-- Update remote table to make listenAndPrint spawnable
remotable ['listenAndPrint]
newRemoteTable :: RemoteTable
newRemoteTable = Main.__remoteTable initRemoteTable


master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- log list of slaves to stderr
  liftIO . hPutStrLn stderr  $ "MASTER :: Found slaves: " ++ show slaves
  liftIO . hPutStrLn stderr $ "MASTER :: Total slaves : " ++ (show $ length slaves)
--  pids <- sequence $ map (flip spawn $ $(mkClosure 'listenAndPrint) ()) slaves
  -- sequence $ map (flip send (Msg "hello")) pids
  -- Test spawn
  say "Spawning local"
  run test
  -- Terminate all the slaves
  terminateAllSlaves backend


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port newRemoteTable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port newRemoteTable
      startSlave backend


type Node = NodeId
data RemoteAlg a = Transfer NodeId a
                 | MyId (String -> a)
                 | RemotePrint String a

instance Functor RemoteAlg where
  fmap f (Transfer nid x) = Transfer nid (f x)
  fmap f (RemotePrint s x) = RemotePrint s (f x)
  fmap f (MyId g) = MyId (f . g)

type Remote = Free RemoteAlg

data Ret a = Value a | Gone deriving (Eq, Show)

run :: Remote a -> Process (Ret a)
run (Pure x) = pure (Value x)
run (Free (Transfer _ rest)) = spawnLocal (run rest >> return ()) >> pure Gone
run (Free (MyId f)) = do
  pid <- getSelfPid
  run (f $ show pid)
run (Free (RemotePrint s rest)) = do
  pid <- getSelfPid
  say ("\n[" ++ show pid ++ "] " ++ s) >> run rest

transfer :: Node -> Remote ()
transfer _ = liftF $ Transfer undefined ()

display :: String -> Remote ()
display s = liftF $ RemotePrint s ()

locid :: Remote String
locid = liftF $ MyId id

test :: Remote ()
test = do
  pid <- locid
  transfer undefined
  display pid
  pid <- locid
  display pid
  return ()
