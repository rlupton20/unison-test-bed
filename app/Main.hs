module Main where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet

import System.IO (hPutStrLn, stderr)
import Control.Monad (forever)

import Control.Monad.Free


master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- log list of slaves to stderr
  say $ "MASTER :: Found slaves: " ++ show slaves
  say $ "MASTER :: Total slaves : " ++ (show $ length slaves)
  -- Run our remote computation
  run test
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


type Node = NodeId
data Task = Task ProcessId

data RemoteAlg a = Transfer NodeId a
                 | MyId (String -> a)
                 | Fork (Remote ()) (Task -> a)
                 | RemotePrint String a

instance Functor RemoteAlg where
  fmap f (Transfer nid x) = Transfer nid (f x)
  fmap f (MyId g) = MyId (f . g)
  fmap f (Fork r g) = Fork r (f . g)
  fmap f (RemotePrint s x) = RemotePrint s (f x)

type Remote = Free RemoteAlg

data Ret a = Value a | Gone deriving (Eq, Show)

run :: Remote a -> Process (Ret a)
run (Pure x) = pure (Value x)
run (Free (Transfer _ rest)) = spawnLocal (run rest >> return ()) >> pure Gone
run (Free (MyId f)) = do
  pid <- getSelfPid
  run (f $ show pid)
run (Free (Fork r rest)) = do
  pid <- spawnLocal $ run r >> return ()
  run ( rest $ Task pid )
run (Free (RemotePrint s rest)) = do
  pid <- getSelfPid
  say s >> run rest

transfer :: Node -> Remote ()
transfer _ = liftF $ Transfer undefined ()

display :: String -> Remote ()
display s = liftF $ RemotePrint s ()

locid :: Remote String
locid = liftF $ MyId id

fork :: Remote () -> Remote Task
fork r = liftF $ Fork r id

forkedTask :: Remote ()
forkedTask = do
  display "Hi there, I'm a forked task"

test :: Remote ()
test = do
  pid <- locid
  transfer undefined
  fork forkedTask
  display pid
  pid <- locid
  display pid

