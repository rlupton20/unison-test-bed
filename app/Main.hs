module Main where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import System.IO (hPutStrLn, stderr)

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- log list of slaves to stderr
  liftIO . hPutStrLn stderr  $ "MASTER :: Found slaves: " ++ show slaves
  liftIO . hPutStrLn stderr $ "MASTER :: Total slaves : " ++ (show $ length slaves)
  -- Terminate all the slaves
  terminateAllSlaves backend


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startSlave backend


