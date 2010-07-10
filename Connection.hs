{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module Connection (
  Connection,
  Callback,
  startConnection,
  connectionId
) where

import Control.Monad( liftM )
import Control.Monad.Maybe
import Control.Monad.Trans( lift )
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception(finally)
import qualified Data.ByteString.UTF8 as Utf8
import Data.List
import Data.Either
import Data.IORef
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Log.Logger(debugM)
import Text.ParserCombinators.Parsec(Parser, parse, try, (<|>))
import Text.ParserCombinators.Parsec.Error

import LocalRouter
import Sasl
import Xml
import qualified XmlParse as XmlParse
import qualified XMPP as Xmpp
import EitherM

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
                                 
type EitherIO a b = IO (Either a b)

data ReadFailure = ParseFailure
                 | InsufficientData
                 deriving (Show, Eq)
                 
type ReadResult = Either ReadFailure
                 
data ConnFailure = AuthFailure
                 deriving (Show, Eq)

type ConnResult = Either ConnFailure
  
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
    loop q state = do
      msg <- atomically $ readTChan q
      rval <- handleMessage msg state 
      case rval of
        Just state' -> loop q state'
        Nothing -> return ()

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

      result <- processStanzas queue (buffer ++ (Utf8.toString input)) state
      case result of 
        Right (leftovers, state') -> do
          writeIORef bufferRef leftovers
          readLoop queue s state'
          
        Left err -> do
          debug $ "Error: " ++ (show err) ++ ", bailing out"
          atomically $ writeTChan q (ProtocolError)

processStanzas :: ConnMsgQ -> String -> ReaderState -> EitherIO ReadFailure (String, ReaderState)
processStanzas q "" state = return $ Right ("", state)
processStanzas q text state@(ReaderState _ _ _ expected) = do
  debug $ "Processing " ++ text
  debug $ "state =  " ++ (show state)
  
  case readXml text (parser state) of
    Right (leftovers, xml) -> do
       response <- processStanza q state xml
       case response of
         Right state' -> processStanzas q leftovers state'
         Left err -> return $ Left err
        
    -- not enough data to read the expected element out of the XML stream...
    -- tell the caller to buffer whatever it is we've got left and go around
    -- again next time data arrives from the client
    Left InsufficientData -> return $ Right (text, state)
    
    -- something nasty happened - pass the error up to the caller
    Left err -> return $ Left err
    
processStanza :: ConnMsgQ -> ReaderState -> XmlElement -> EitherIO ReadFailure ReaderState
processStanza q state (XmlProcessingInstruction _) =  return $ Right state
processStanza q state@(ReaderState _ _ _ expected) xml = do
  debug $ "xml: " ++ (show xml)
  
  let expected' = if expected == Preamble then StreamStart else expected 
  let state' = state {rdrExpect = expected'}
  
  case Xmpp.fromXml xml of 
    Just stanza -> do
      debug $ "stanza: " ++ (show stanza)
      let rVar = rdrCondVar state
      atomically $ writeTChan q (InboundXmpp stanza rVar)
      debug $ "Waiting for response..."
      rval <- takeMVar rVar
      debug $ "Response was " ++ (show rval)
      
      return $ processResponse state rval
      
    Nothing -> do
      debug $ "parse failure"
      return $ Left ParseFailure 
  where 
    processResponse :: ReaderState -> [Response] -> Either ReadFailure ReaderState
    processResponse s r = Right $ foldl' updateState s r
     
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
          
handleMessage :: ConnMsg -> ConnectionState -> IO (Maybe ConnectionState)
handleMessage (InboundXmpp (Xmpp.RxStreamOpen _ _) sig) state = do
  debug $ "received stream open block"
  let s = stateSocket state
  xmppSend s $ Xmpp.TxStreamOpen "localhost" (idNumber state)
  xmppSend s $ xmppFeatures
  putMVar sig [Ok, Connection.Expect Any]
  return (Just state)

handleMessage (InboundXmpp (Xmpp.AuthMechanism m) sig) state = do
  debug $ "received authentication selection request: " ++ m
  auth <- newAuthInfo m "jhabber"
  case auth of
    Just a -> do 
      xmppSend (stateSocket state) $ Xmpp.AuthChallenge (challenge a)
      let state' = state { authInfo = a }
      putMVar sig [Ok]
      return $ Just state'
    Nothing -> do
      -- unsupported authentication mechanism. Bail.
      return Nothing

handleMessage (InboundXmpp (Xmpp.AuthResponse r) sig) state = do
  debug $ "received authentication response " ++ (show $ authInfo state) 
  result <- case checkResponse (authInfo state) r of
    Right (authInfo', authAction) -> do
      newStateE <- sendAuthResponse state authInfo' authAction
      case newStateE of
        Right state' -> return $ Right (state', authAction)
        Left e -> return $ Left AuthFailure
    Left _ -> return $ Left AuthFailure

  case result of 
    Right (s, a) -> do 
      let expected = if a == Success then StreamStart else Any
      putMVar sig [Ok, Connection.Expect expected]
      return $ Just s
      
    Left _ -> do
      authFailure state "temporary-auth-failure"
      putMVar sig [Quit]
      return Nothing
          
  {-
  case of
    Right (authInfo', rval) -> 
      r <- generateResponse state authInfo' rval
      case r of 
        Right state' -> do 
          putMVar sig [Ok, Connection.Expect StreamStart]
          return $ Just state'
          
        Left _ -> do
          authFailure state "temporary-auth-failure"
          putMVar sig [Quit]
          return Nothing
    Left _ -> do
      authFailure state "temporary-auth-failure"
      putMVar sig [Quit]
      return Nothing
  -}
      
handleMessage m s = return (Just s)

sendAuthResponse :: ConnectionState -> AuthInfo -> AuthResponse -> EitherIO ConnFailure ConnectionState
sendAuthResponse state auth response = do
  debug $ "auth: " ++ (show response)
  let sock = stateSocket state
  case response of
    NeedsAuthentication -> 
      authenticate state auth
    Challenge s -> do 
      xmppSend sock $ Xmpp.AuthChallenge s 
      return $ Right state { authInfo = auth } 
    Success -> do
      xmppSend sock $ Xmpp.AuthSuccess
      return $ Right state { authInfo = auth }

authenticate :: ConnectionState -> AuthInfo -> EitherIO ConnFailure ConnectionState
authenticate state authInfo = do
  -- TODO: lookup this user's *actual* password here
  let pwd = "pwd"
  let uid = getUid authInfo

  case checkCredentials authInfo uid pwd of
    Right (authInfo', Challenge rval) -> do
      xmppSend (stateSocket state) $ Xmpp.AuthChallenge rval
      return (Right $ state { authInfo = authInfo' } )
      
    Left _ -> do
      authFailure state "temporary-auth-failure"
      return $ Left AuthFailure

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

debug = debugM "conn"