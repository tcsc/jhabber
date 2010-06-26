module ConnectionManager (ConnectionManager,
                          newConnectionManager,
                          createConnection) where
  
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad(liftM)
import Data.IORef
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Network.BSD
import Network.Socket(Socket)
import qualified Network.Socket.ByteString as SBS
import System.Log.Logger(debugM)
import Text.ParserCombinators.Parsec(Parser, parse)
import Text.ParserCombinators.Parsec.Error

import LocalRouter
import XmlParse
import XMPP

data Connection = Conn { connId :: Integer,
                         connMgr :: ConnectionManager,
                         connSocket :: Socket, 
                         connBuffer :: (IORef String),
                         connRouter :: Router }
                         
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
createConnection :: ConnectionManager -> Socket -> IO Connection
createConnection mgr@(Mgr r _) s = do
  newId <- atomically $ getNewId mgr
  c <- startConnection mgr r newId s 
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
  
modifyTVar :: TVar a -> (a -> a) -> STM () 
modifyTVar m f = do 
  v <- readTVar m
  writeTVar m $ f v
  
debugMgr = debugM "Mgr"
  
{- ------------------------------------------------------------------------- -}
  
startConnection :: ConnectionManager -> Router -> Integer -> Socket -> IO Connection 
startConnection mgr r cId s = do
  buffer <- newIORef ""
  let c = Conn { connId = cId, 
                 connMgr = mgr, 
                 connRouter = r, 
                 connSocket = s, 
                 connBuffer = buffer }
  forkIO $ runConnection c
  return c

runConnection :: Connection -> IO ()
runConnection c  = do
  debug $ "Starting connection #" ++ (show $ connId c)
  start <- readConnection c xmppParseStreamStart 
  case start of
    Just xml -> do 
      xmppSend c $ RxStreamOpen "localhost" (connId c)
      xmppSend c $ xmppFeatures
      connLoop c
    Nothing -> do
      debug "parse fail"
      return ()
  
connLoop :: Connection -> IO ()
connLoop c = do
  rval <- readConnection c xmppParseStanza 
  case rval of
    Nothing -> do 
      debug "Failure - abanoning connection"
      return ()
      
    Just xml -> do
      debug $ "xml: " ++ (show xml)
      connLoop c

readConnection :: Connection -> Parser a -> IO (Maybe a)
readConnection c parser = do
  let bufferRef = connBuffer c
  buffer <- readIORef bufferRef
  input <- (liftM toString) $ SBS.recv (connSocket c) 1024
  
  case length input of
    0 -> do -- closed connection
      debug "Connection looks like its closed"
      return Nothing
      
    _ -> do
      case parse (xmlGetRemainder . xmlTrim $ parser) "" (buffer ++ input) of
        Right (result, remainder) -> do
          writeIORef bufferRef remainder
          return (Just result)
          
        Left e -> do
          case head (errorMessages e) of
            SysUnExpect [] -> do
              debug "Unexpected end-of-input"
              writeIORef bufferRef (buffer ++ input)
              readConnection c parser
          
            _ -> do -- faility fail-fail
              debug $ "Parse failure: " ++ (show e)
              return Nothing
              
xmppSend :: Connection -> XmppStanza -> IO ()
xmppSend c stanza = do
  rval <- SBS.send (connSocket c) $ xmppFormat stanza
  -- error handling goes here 
  return ()
  
xmppFeatures :: XmppStanza 
xmppFeatures = Features [
  (XmlElement "" "bind" [XmlAttribute "" "xmlns" "urn:ietf:params:xml:ns:xmpp-bind"] []),
  (XmlElement "" "session" [XmlAttribute "" "xmlns" "urn:ietf:params:xml:ns:xmpp-session"] []),
  (XmlElement "" "mechanisms" [XmlAttribute "" "xmlns" "urn:ietf:params:xml:ns:xmpp-sasl"] xmppMechanisms)
  ]
  
xmppMechanisms :: [XmlElement]
xmppMechanisms = [
  (XmlElement "" "mechanism" [] [XmlText "DIGEST-MD5"]), 
  (XmlElement "" "mechanism" [] [XmlText "PLAIN"]) ]
  
debug = debugM "conn"