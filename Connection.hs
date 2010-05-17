module Connection( Connection,
                   startConnection) where
  
import Network.Socket
import Control.Concurrent

data Connection = MkConn { connSocket :: Socket }
  
startConnection :: Socket -> IO Connection
startConnection s = do
  let c = MkConn s
  forkIO $ connLoop c
  return c
  
connLoop :: Connection -> IO ()
connLoop c = do
  str <- recv (connSocket c) 1024
  connLoop c 