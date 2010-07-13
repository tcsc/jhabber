{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module Connection 

where

  {- (
    Connection,
    Callback,
    startConnection,
    connectionId
  ) -}

import Control.Monad( liftM )
import Control.Monad.Maybe
import Control.Monad.Trans( lift )
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception(finally)
import Control.Monad.Error
import qualified Data.ByteString.UTF8 as Utf8
import Data.List
import Data.Either
import Data.IORef
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified System.Log.Logger as L
import Text.ParserCombinators.Parsec(Parser, parse, try, (<|>))
import Text.ParserCombinators.Parsec.Error

import LocalRouter
import Sasl
import Xml
import qualified XmlParse as XmlParse
import qualified XMPP as Xmpp

{-| 
 A signal to the reader of what it should expect from the XML stream from next
 read
 -}
data ElementType = Preamble
                 | StreamStart
                 | Any
                 deriving (Eq, Show)

{-| 
 The response values that the main connection thread can send back to the
 reader thread after receiving an XMPP stanza.
 -}
data Response = Expect ElementType
              | Pipeline Bool
              | Quit
              | Ok
              deriving (Show)

type ResponseVar = MVar [Response]

data ConnMsg = StreamStartRx
             | InboundXmpp Xmpp.Stanza ResponseVar
             | OutboundXmpp Xmpp.Stanza
             | ConnectionLost
             | ProtocolError
             | Close

type ConnMsgQ = TChan ConnMsg

data Connection = Conn Integer ConnMsgQ
                         
type Callback = Connection -> IO ()

data ConnectionState = State { idNumber :: Integer,
                               stateSocket :: Socket,
                               router :: Router,
                               onLostCB :: Callback,
                               msgQ :: ConnMsgQ,
                               authInfo :: AuthInfo }

data ReaderState = ReaderState { rdrCondVar :: ResponseVar,
                                 rdrPipeline :: Bool,
                                 rdrBuffer :: IORef String,
                                 rdrExpect :: ElementType } 

instance Show (ReaderState) where
  show (ReaderState _ _ _ ex) = "Expected: " ++ (show ex)   
                                 
data ReadFailure = ParseFailure
                 | InsufficientData
                 deriving (Read, Show, Eq)
instance Error ReadFailure where 
  strMsg x = read x
  
type ReadResult a = Either ReadFailure a
type ReadResultIO a = ErrorT ReadFailure IO a

data ConnFailure = AuthFailure
                 | UnsupportedMechanism
                 deriving (Read, Show, Eq)
instance Error ConnFailure where
  strMsg x = read x
type ConnResult a = Either ConnFailure a
type ConnResultIO a = ErrorT ConnFailure IO a 

  
{- ------------------------------------------------------------------------- -}

startConnection :: Router -> Integer -> Socket -> Callback -> IO Connection 
startConnection r cId s onLost = do
  q <- newTChanIO
  let c = Conn cId q
  let state = State { idNumber = cId, 
                      stateSocket = s,
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
    let sock = stateSocket s
    forkIO $ runReader (msgQ s) sock
    loop (msgQ s) s `finally` sClose sock
  where
    loop :: ConnMsgQ -> ConnectionState -> IO ()
    loop q state = do
      msg <- atomically $ readTChan q
      rval <- runErrorT $ handleMessage msg state 
      case rval of
        Right state' -> loop q state'
        Left e -> do
          debug $ "Message processing failure: " ++ (show e)
          loop q state

runReader :: ConnMsgQ -> Socket -> IO ()
runReader q s = do
    b <- newIORef ""
    cv <- newEmptyMVar
    let state = ReaderState { rdrCondVar = cv, 
                              rdrPipeline = False,
                              rdrBuffer = b,
                              rdrExpect = Preamble }
    readLoop q s state
  where
    readLoop :: ConnMsgQ -> Socket -> ReaderState -> IO ()
    readLoop queue socket state@(ReaderState _ _ bufferRef _) = do
      buffer <- readIORef bufferRef
      input <- recv socket 1024

      result <- runErrorT $ processStanzas queue (buffer ++ (Utf8.toString input)) state
      case result of 
        Right (leftovers, state') -> do
          writeIORef bufferRef leftovers
          readLoop queue s state'
          
        Left err -> do
          debug $ "Error: " ++ (show err) ++ ", bailing out"
          atomically $ writeTChan q (ProtocolError)

processStanzas :: ConnMsgQ -> String -> ReaderState -> ReadResultIO (String, ReaderState)
processStanzas q "" state = return ("", state)
processStanzas q text state@(ReaderState _ _ _ expected) = do
  (lift . debug) $ "Processing " ++ text
  (lift . debug) $ "state =  " ++ (show state)
  
  case readXml text (parser state) of
    Right (leftovers, xml) -> do
       state' <- processStanza q state xml
       processStanzas q leftovers state'
        
    -- not enough data to read the expected element out of the XML stream...
    -- tell the caller to buffer whatever it is we've got left and go around
    -- again next time data arrives from the client
    Left InsufficientData -> return (text, state)
    
    -- something nasty happened - pass the error up to the caller
    Left _ -> throwError ParseFailure

{-|
  Proceses an individual XML block
 -}
processStanza :: ConnMsgQ -> ReaderState -> XmlElement -> ReadResultIO ReaderState
processStanza q state (XmlProcessingInstruction _) =  return state
processStanza q state@(ReaderState _ _ _ expected) xml = do
  (lift . debug) $ "xml: " ++ (show xml)
  
  let expected' = if expected == Preamble then StreamStart else expected 
  let state' = state {rdrExpect = expected'}
  
  case Xmpp.fromXml xml of 
    Just stanza -> do
      (lift . debug) $ "stanza: " ++ (show stanza)
      let rVar = rdrCondVar state
      (lift . atomically) $ writeTChan q (InboundXmpp stanza rVar)
      (lift . debug) $ "Waiting for response..."
      rval <- (lift . takeMVar) rVar
      (lift . debug) $ "Response was " ++ (show rval)
      
      return $ processResponse state rval
    -- this is just an xml-to-xmpp translation failure, which shouldn't
    -- necessarily be a fatal error... just return the existing state and carry
    -- on
    Nothing -> return state
  where 
    processResponse :: ReaderState -> [Response] -> ReaderState
    processResponse s r = foldl' updateState s r
     
    updateState :: ReaderState -> Response -> ReaderState
    updateState s (Connection.Expect expected) = s {rdrExpect = expected}
    updateState s _ = s

{-|
 reads an XML element out out of the supplied string and returns both it and
 the remainder of the string
 -}
readXml :: String -> Parser XmlElement -> ReadResult (String, XmlElement)
readXml text p = 
  case parse (XmlParse.getRemainder p) "" text of
    Right (xml, remainder) -> Right (remainder, xml)
    Left err -> case head (errorMessages err) of
      -- simple out-of-data error. No biggie, just go around and try again
      SysUnExpect [] -> Left InsufficientData
        
      -- full-on parse failure... we should do something here 
      _ -> Left ParseFailure
        
parser :: ReaderState -> Parser XmlElement
parser (ReaderState _ _ _ expected) = 
  XmlParse.trim $ case expected of 
    Preamble -> (try XmlParse.processingInstruction) <|> XmlParse.simpleTag
    StreamStart -> XmlParse.simpleTag
    _ -> XmlParse.nestedTag
          
handleMessage :: ConnMsg -> ConnectionState -> ConnResultIO ConnectionState
handleMessage (InboundXmpp (Xmpp.RxStreamOpen _ _) sig) state = do
  debugM $ "received stream open block"
  let s = stateSocket state
  liftIO $ xmppSend s $ Xmpp.TxStreamOpen "localhost" (idNumber state)
  liftIO $ xmppSend s $ xmppFeatures
  liftIO $ putMVar sig [Ok, Connection.Expect Any]
  return state

handleMessage (InboundXmpp (Xmpp.AuthMechanism m) sig) state = do
  debugM $ "received authentication selection request: " ++ m
  auth <- liftIO $ newAuthInfo m "jhabber"
  case auth of
    Just a -> do 
      liftIO $ xmppSend (stateSocket state) $ Xmpp.AuthChallenge (challenge a)
      let state' = state { authInfo = a }
      liftIO $ putMVar sig [Ok]
      return state'
    Nothing -> throwError UnsupportedMechanism

handleMessage (InboundXmpp (Xmpp.AuthResponse r) sig) state = do
  debugM $ "received authentication response " ++ (show $ authInfo state)
  case checkResponse (authInfo state) r of
    Right (authInfo', authAction) -> do 
      state' <- handleAuthResponse state authInfo' authAction
      let expected = if authAction == Success then StreamStart else Any
      liftIO $ putMVar sig [Ok, Connection.Expect expected]
      return state'
    Left _ -> throwError AuthFailure
  `catchError` \e -> do
    liftIO $ debug $ "Error handling auth response: " ++ (show e)
    case e of 
      AuthFailure -> do
        liftIO $ authFailure state "temporary-auth-failure"
        liftIO $ putMVar sig [Quit]
      _ -> return ()
    throwError e

handleMessage m s = return s

handleAuthResponse :: ConnectionState -> AuthInfo -> AuthResponse -> ConnResultIO ConnectionState
handleAuthResponse state auth response = do
  debugM $ "auth: " ++ (show response)
  let sock = stateSocket state
  case response of
    NeedsAuthentication -> authenticate state auth
    Challenge s -> do 
      liftIO $ xmppSend sock $ Xmpp.AuthChallenge s 
      return state { authInfo = auth } 
    Success -> do
      liftIO $ xmppSend sock $ Xmpp.AuthSuccess
      return state { authInfo = auth }

authenticate :: ConnectionState -> AuthInfo -> ConnResultIO ConnectionState
authenticate state authInfo = do
  -- TODO: lookup this user's *actual* password here
  let pwd = "__badpwd__"
  let uid = getUid authInfo

  case checkCredentials authInfo uid pwd of
    Right (authInfo', Challenge rval) -> do
      liftIO $ xmppSend (stateSocket state) $ Xmpp.AuthChallenge rval
      return state { authInfo = authInfo' }
      
    Left _ -> throwError AuthFailure

authFailure :: ConnectionState -> String -> IO ()
authFailure state reason = xmppSend (stateSocket state)  $ Xmpp.newAuthFailure reason

{-|
 Formats an XMPP stanza and sends it out on the socket
 -}
xmppSend :: Socket -> Xmpp.Stanza -> IO ()
xmppSend socket stanza = do
  rval <- send socket $ Xmpp.format stanza
  -- error handling goes here 
  return ()

xmppFeatures :: Xmpp.Stanza 
xmppFeatures = Xmpp.Features [
  (XmlElement "" "bind" [XmlAttribute "" "xmlns" "urn:ietf:params:xml:ns:xmpp-bind"] []),
  (XmlElement "" "session" [XmlAttribute "" "xmlns" "urn:ietf:params:xml:ns:xmpp-session"] []),
  (XmlElement "" "mechanisms" [XmlAttribute "" "xmlns" "urn:ietf:params:xml:ns:xmpp-sasl"] xmppMechanisms)
  ]

xmppMechanisms :: [XmlElement]
xmppMechanisms = [
  (XmlElement "" "mechanism" [] [XmlText "DIGEST-MD5"]), 
  (XmlElement "" "mechanism" [] [XmlText "PLAIN"]) ]

debugM s = liftIO $ debug s

debug :: String -> IO ()
debug = L.debugM "conn"