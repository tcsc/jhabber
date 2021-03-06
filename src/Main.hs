module Main where

import IO
import Control.Concurrent
import Network
import Network.Socket
import System.Log.Logger
import System.Log.Handler.Simple

import Database
import ConnectionManager
import Listener
import LocalRouter

data State = State {
  stateRouter :: Router,
  stateManager :: ConnectionManager,
  stateListeners :: [Listener]
}

main :: IO ()
main = withSocketsDo $ do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  vl <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [vl])

  debug "Entering main"
  r <- newRouter 4
  db <- startDb "127.0.0.1" "jhabber"
  mgr <- newConnectionManager r
  l <- startlistener (newConnection mgr db) 4321
  mainloop State { stateRouter = r, stateManager = mgr, stateListeners = [l] }

mainloop :: State -> IO ()
mainloop s = do
  threadDelay 1000
  mainloop s
  {-
  x <- getChar
  case x of
    'q' -> Main.shutdown s
    'c' -> do LocalRouter.crashRouter $ stateRouter s
              mainloop s
    _   -> mainloop s
    -}

newConnection :: ConnectionManager -> JhabberDb -> Socket -> IO ()
newConnection mgr db s = do
  debug "new connection" -- conn <- newConnection s
  createConnection mgr db s
  return ()

shutdown :: State -> IO ()
shutdown s = do
  stopRouter $ stateRouter s
  return ()

debug = debugM "main"
