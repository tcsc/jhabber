module ConnectionManager (ConnectionManager,
                          newConnectionManager,
                          createConnection) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Network.Socket(Socket)
import qualified Network.Socket.ByteString as SBS
import System.Log.Logger(debugM)


import Connection(Connection, startConnection, connectionId)
import Database(JhabberDb, UserInfo, lookupUser)
import LocalRouter
import XmlParse
import Xmpp

data MgrState = State { mgrConnections :: [Connection],
                        mgrIdSeed :: Integer }

data ConnectionManager = Mgr Router (TVar MgrState)

{- Creates a new connection manager -}
newConnectionManager :: Router -> IO ConnectionManager
newConnectionManager r = do
  state <- atomically $ newTVar (State [] 0)
  return (Mgr r state)

{-
Creates a new connection object and remembers it. Also spawns a new thread for
handling all of the connection I/O
-}
createConnection :: ConnectionManager -> JhabberDb -> Socket -> IO Connection
createConnection mgr@(Mgr r _) db s = do
  newId <- atomically $ getNewId mgr
  debug $ "Starting connection #" ++ (show newId)
  c <- startConnection r newId db s (connectionLost mgr)
  atomically $ rememberConnection mgr c
  return c

getNewId :: ConnectionManager -> STM Integer
getNewId (Mgr _ state) = do
  (State conns seed) <- readTVar state
  let newId = seed + 1
  writeTVar state $ State conns newId
  return newId

rememberConnection :: ConnectionManager -> Connection -> STM ()
rememberConnection (Mgr _ state) c =
  modifyTVar state (\(State l idSeed) -> State (c:l) idSeed)

connectionLost :: ConnectionManager -> Connection -> IO ()
connectionLost mgr conn = do
  debug $ "Connection " ++ (show $ connectionId conn) ++ " lost"
  return ()

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar m f = do
  v <- readTVar m
  writeTVar m $ f v

debug = debugM "Mgr"

