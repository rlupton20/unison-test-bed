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
  pids <- sequence $ map (flip spawn $ $(mkClosure 'listenAndPrint) ()) slaves
  sequence $ map (flip send (Msg "hello")) pids
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
