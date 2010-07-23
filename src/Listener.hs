module Listener ( startlistener,
                  Listener ) where

--import IO
import Network
import Network.BSD
import Network.Socket as Socket
import Control.Exception (bracket, bracketOnError, throwTo, Exception)
import Control.Concurrent
import Data.Typeable
import System.Log.Logger

data Listener = MkListener Socket ThreadId

startlistener :: (Socket -> IO ()) -> PortNumber -> IO Listener
startlistener f port = do
  s <- createSocket iNADDR_ANY 4321
  t <- forkIO $ listener s f port
  return (MkListener s t)

listener :: Socket -> (Socket -> IO ()) -> PortNumber -> IO ()
listener socket f port = do
  debug "Entering listener"
  listenLoop f socket

listenLoop :: (Socket -> IO ()) -> Socket -> IO ()
listenLoop f s = do
  (client, address) <- Socket.accept s
  f client
  listenLoop f s

-- | Creates the server socket for the listener thread
createSocket :: HostAddress -> PortNumber -> IO Socket
createSocket addr port = do
  protocol <- getProtocolNumber "tcp"
  bracketOnError
    (socket AF_INET Stream protocol)
    (sClose)
    (\sock -> do
      bindSocket sock (SockAddrInet port addr)
      listen sock maxListenQueue
      return sock)

debug :: String -> IO ()
debug msg = debugM "listener" msg
