module ConnectionManager (ConnectionManager,
                          newConnectionManager,
                          createConnection) where
  
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Connection
import Network.BSD
import Network.Socket as Socket

data ConnectionManager = Mgr (TVar [Connection])

{- Creates a new connection manager -}
newConnectionManager :: IO ConnectionManager
newConnectionManager = do 
  mgr <- atomically $ newTVar []
  return (Mgr mgr)

{- 
Creates a new connection object and remembers it. Also spawns a new thread for 
handling all of the connection I/O 
-}
createConnection :: ConnectionManager -> Socket -> IO Connection
createConnection (Mgr ctl) s = do
  c <- startConnection s
  atomically $ modifyTVar ctl (\l -> c : l)
  return c
  
modifyTVar :: TVar a -> (a -> a) -> STM () 
modifyTVar m f = do 
  v <- readTVar m
  writeTVar m $ f v 