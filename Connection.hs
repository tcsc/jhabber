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
import XmlParse
import XMPP

data ConnMsg = StreamStartRx
             | InboundXmpp XmppStanza
             | OutboundXmpp XmppStanza
             | ConnectionLost
             | ProtocolError
             | Close

type ConnMsgQ = TChan ConnMsg

data Connection = Conn Integer ConnMsgQ
                         
type Callback = Connection -> IO ()

data ConnectionState = State { idNumber :: Integer,
                               socket :: Socket,
                               router :: Router,
                               onLostCB :: Callback,
                               msgQ :: ConnMsgQ }  
  
{- ------------------------------------------------------------------------- -}

startConnection :: Router -> Integer -> Socket -> Callback -> IO Connection 
startConnection r cId s onLost = do
  q <- newTChanIO
  let c = Conn cId q
  let state = State { idNumber = cId, 
                      socket = s,
                      router = r,
                      msgQ = q, 
                      onLostCB = onLost }
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
  case m of 
    "DIGEST-MD5" -> do
      let s = socket state
      xmppSend s $ AuthChallenge (digestChallenge "jhabber" "n0nc3!")
      return (Just state)
    _ -> return Nothing
    
handleMessage m s = return (Just s)

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
              
digestChallenge :: String -> String -> String
digestChallenge r n =
  "realm=\"" ++ r ++ "\"," ++ 
  "nonce=\"" ++ n ++ "\"," ++ 
  "qop=\"auth\"," ++ 
  "charset=\"utf-8\"," ++
  "algorithm=\"md5-sess\""

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