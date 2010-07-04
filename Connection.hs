module Connection (
  Connection,
  Callback,
  startConnection,
  connectionId
) where

import Control.Monad( liftM )
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.ByteString.UTF8 (toString)
import Data.IORef
import Network.BSD
import Network.Socket(Socket)
import qualified Network.Socket.ByteString as SBS
import System.Log.Logger(debugM)
import Text.ParserCombinators.Parsec(Parser, parse)
import Text.ParserCombinators.Parsec.Error

import LocalRouter
import Sasl
import XmlParse
import XMPP

data ConnMsg = StreamStartRx
             | InboundXmpp XmppStanza
             | OutboundXmpp XmppStanza
             | ConnectionLost
             | ProtocolError
             | Close
             deriving (Show)

type ConnMsgQ = TChan ConnMsg

data Connection = Conn Integer ConnMsgQ
                         
type Callback = Connection -> IO ()

data ConnectionState = State { idNumber :: Integer,
                               socket :: Socket,
                               router :: Router,
                               onLostCB :: Callback,
                               msgQ :: ConnMsgQ,
                               authInfo :: AuthInfo }  
  
{- ------------------------------------------------------------------------- -}

startConnection :: Router -> Integer -> Socket -> Callback -> IO Connection 
startConnection r cId s onLost = do
  q <- newTChanIO
  let c = Conn cId q
  let state = State { idNumber = cId, 
                      socket = s,
                      router = r,
                      msgQ = q, 
                      onLostCB = onLost,
                      authInfo = None }
  forkIO $ runConnection state
  return c
  
connectionId :: Connection -> Integer
connectionId (Conn id _) = id

runConnection :: ConnectionState -> IO ()
runConnection s  = do
    debug $ "Starting connection #" ++ (show $ idNumber s)
    forkIO $ runReader (msgQ s) (socket s)
    loop (msgQ s) s
  where
    loop q state = do
      msg <- atomically $ readTChan q
      rval <- handleMessage msg state 
      case rval of
        Just state' -> loop q state'
        Nothing -> return ()

runReader :: ConnMsgQ -> Socket -> IO ()
runReader q s = do
  b <- newIORef ""
  start <- readConnection s b xmppParseStreamStart 
  case start of
    Just xml -> do 
      atomically $ writeTChan q StreamStartRx
      readLoop q s b
      
    Nothing -> do 
      atomically $ writeTChan q ProtocolError
      return()
    
  where
    readLoop queue socket buffer = do
      rval <- readConnection socket buffer xmppParseStanza 
      case rval of 
        Just xml -> do
          -- debug $ "xml: " ++ (show xml) ++ "\n"
          case xmppFromXml xml of
            Just stanza -> do
              atomically $ writeTChan q (InboundXmpp stanza) 
            Nothing  -> return ()
          readLoop queue socket buffer
        
        Nothing -> do
          atomically $ writeTChan q ProtocolError
          
handleMessage :: ConnMsg -> ConnectionState -> IO (Maybe ConnectionState)
handleMessage StreamStartRx state = do
  debug $ "received stream open block"
  let s = socket state
  xmppSend s $ RxStreamOpen "localhost" (idNumber state)
  xmppSend s $ xmppFeatures
  return (Just state)

handleMessage (InboundXmpp (AuthMechanism m)) state = do
  debug $ "received authentication selection request: " ++ m
  auth <- newAuthInfo m "jhabber"
  case auth of
    Just a ->  do 
      xmppSend (socket state) $ AuthChallenge (challenge a)
      let state' = state { authInfo = a }
      return $ Just state'
    Nothing -> do
      -- unsupported authentication mechanism. Bail.
      return Nothing

handleMessage (InboundXmpp (AuthResponse r)) state = do
  debug $ "received authentication response " ++ (show $ authInfo state)
  case checkResponse (authInfo state) r of 
    Just (authInfo', rval) ->
      generateResponse state authInfo' rval
          
    Nothing -> do
      authFailure state "temporary-auth-failure" 
      return Nothing

handleMessage m s = return (Just s)

generateResponse :: ConnectionState -> AuthInfo -> AuthResponse -> IO (Maybe ConnectionState)
generateResponse state auth response = do
  debug $ "auth: " ++ (show response)
  case response of
    NeedsAuthentication -> 
      authenticate state auth
    Challenge s -> do 
      xmppSend (socket state) $ AuthChallenge s 
      return $ Just state { authInfo = auth } 
    Success -> do
      xmppSend (socket state) $ AuthSuccess
      return $ Just state { authInfo = auth }

authenticate :: ConnectionState -> AuthInfo -> IO (Maybe ConnectionState)
authenticate state authInfo = do
  -- TODO: lookup this user's *actual* password here
  let pwd = "pwd"
  let uid = getUid authInfo

  case checkCredentials authInfo uid pwd of
    Just (authInfo', Challenge rval) -> do
      xmppSend (socket state) $ AuthChallenge rval
      return (Just $ state { authInfo = authInfo' } )
      
    Nothing -> do
      authFailure state "temporary-auth-failure"
      return Nothing

authFailure :: ConnectionState -> String -> IO ()
authFailure state reason = xmppSend (socket state)  $ xmppNewAuthFailure reason

{-|
 Reads an element from the connection and returns it to the caller
 -}
readConnection :: Socket -> IORef String -> Parser a -> IO (Maybe a)
readConnection s bufferRef parser = do
  buffer <- readIORef bufferRef
  input <- (liftM toString) $ SBS.recv s 1024

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
              readConnection s bufferRef parser

            _ -> do -- faility fail-fail
              debug $ "Parse failure: " ++ (show e)
              return Nothing

{-|
 Formats an XMPP stanza and sends it out on the socket
 -}
xmppSend :: Socket -> XmppStanza -> IO ()
xmppSend socket stanza = do
  rval <- SBS.send socket $ xmppFormat stanza
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