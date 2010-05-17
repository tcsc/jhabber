module Main where
  
import IO
import Network
import Network.Socket
import System.Log.Logger
import System.Log.Handler.Simple

import Listener
import LocalRouter 

main :: IO ()
main = withSocketsDo $ do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  vl <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [vl])
  
  
  debug "Entering main"
  r <- newRouter 4
  l <- startlistener (\s -> do debug "new connection") 4321 
  mainloop r l
  
mainloop :: Router -> Listener -> IO ()
mainloop r l = do
  x <- getChar
  case x of
    'q' -> Main.shutdown r l
    'c' -> do LocalRouter.crashRouter r
              mainloop r l
    _   -> mainloop r l

shutdown :: Router -> Listener -> IO ()
shutdown r l = do 
  stopRouter r
  return () 

debug = debugM "main" 