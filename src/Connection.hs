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
import Data.Maybe
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified System.Log.Logger as L
import Text.ParserCombinators.Parsec(Parser, parse, try, (<|>))
import Text.ParserCombinators.Parsec.Error

import Database
import Sasl
import Xml
import qualified XmlParse as XmlParse
import qualified Xmpp as Xmpp
import LocalRouter

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
             | SendMessage JID InstantMessage

type ConnMsgQ = TChan ConnMsg

data Connection = Conn Integer ConnMsgQ

type Callback = Connection -> IO ()

data ConnState = State { idNumber :: Integer,
                         stateSocket :: Socket,
                         router :: Router,
                         onLostCB :: Callback,
                         msgQ :: ConnMsgQ,
                         authInfo :: AuthInfo,
                         database :: JhabberDb,
                         stateJid :: Xmpp.JID }

data ReaderState = ReaderState { rdrCondVar :: ResponseVar,
                                 rdrPipeline :: Bool,
                                 rdrBuffer :: IORef String,
                                 rdrExpect :: ElementType,
                                 rdrContext :: !XmlParse.Context }

instance Show (ReaderState) where
  show (ReaderState _ _ _ ex _) = "Expected: " ++ (show ex)

data ReadFailure = ParseFailure
                 | InsufficientData
                 deriving (Read, Show, Eq)
instance Error ReadFailure where
  strMsg x = read x

type ReadResult a = Either ReadFailure a
type ReadResultIO a = ErrorT ReadFailure IO a

data ConnFailure = AccessDenied
                 | AuthFailure
                 | BadRequest
                 | BindConflict
                 | NotAuthenticated
                 | NotFound
                 | XmppFailure
                 | Timeout
                 | Unsupported
                 | UnknownFailure
                 | UnsupportedMechanism
                 | ServiceUnavailable
                 deriving (Read, Show, Eq)

data ErrorType = ErrTryAgain | ErrCancel

instance Error ConnFailure where
  strMsg x = read x

type ConnResult a = Either ConnFailure a
type ConnResultIO a = ErrorT ConnFailure IO a

type XmppResultIO a = ErrorT Xmpp.Failure IO a


localhost = "localhost"

{- ------------------------------------------------------------------------- -}

startConnection :: Router -> Integer -> JhabberDb -> Socket -> Callback -> IO Connection
startConnection r cId db s onLost = do
  q <- newTChanIO
  let c = Conn cId q
  let state = State { idNumber = cId,
                      stateSocket = s,
                      router = r,
                      msgQ = q,
                      onLostCB = onLost,
                      authInfo = None,
                      database = db,
                      stateJid = (Xmpp.JID "" "" "") }
  forkIO $ runConnection state
  return c

{- ------------------------------------------------------------------------- -}

connectionId :: Connection -> Integer
connectionId (Conn id _) = id

{- ------------------------------------------------------------------------- -}

runConnection :: ConnState -> IO ()
runConnection s  = do
    debug $ "Starting connection #" ++ (show $ idNumber s)
    let sock = stateSocket s
    forkIO $ runReader (msgQ s) sock
    loop (msgQ s) s `finally` sClose sock
  where
    loop :: ConnMsgQ -> ConnState -> IO ()
    loop q state = do
      msg <- atomically $ readTChan q
      rval <- runErrorT $ handleMessage msg state
      case rval of
        Right state' -> loop q state'
        Left e -> do
          debug $ "Message processing failure: " ++ (show e)
          loop q state

{- ------------------------------------------------------------------------- -}

runReader :: ConnMsgQ -> Socket -> IO ()
runReader q s = do
    b <- newIORef ""
    cv <- newEmptyMVar
    let state = ReaderState { rdrCondVar = cv,
                              rdrPipeline = False,
                              rdrBuffer = b,
                              rdrExpect = Preamble,
                              rdrContext = XmlParse.newContext }
    readLoop q s state
  where
    readLoop :: ConnMsgQ -> Socket -> ReaderState -> IO ()
    readLoop queue socket state = do
      let bufferRef = rdrBuffer state
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

{- -------------------------------------------------------------------------- -}

processStanzas :: ConnMsgQ -> String -> ReaderState -> ReadResultIO (String, ReaderState)
processStanzas q "" state = return ("", state)
processStanzas q text state =  do
  debugM $ "Processing " ++ text
  debugM $ "state =  " ++ (show state)

  case readXml state text of
    Right (leftovers, xml, state') -> do
      state'' <- processStanza q state' xml
      processStanzas q leftovers state''

    -- we're at the end of the document preamble. Change the expected value to
    -- "stream start" and try again
    Left XmlParse.NoMorePreamble ->
      processStanzas q text (state { rdrExpect = StreamStart })

    -- not enough data to read the expected element out of the XML stream...
    -- tell the caller to buffer whatever it is we've got left and go around
    -- again next time data arrives from the client
    Left XmlParse.InsufficientData -> return (text, state)

    -- something nasty happened - pass the error up to the caller
    Left _ -> throwError ParseFailure

{- -------------------------------------------------------------------------- -}

readXml :: ReaderState -> String -> XmlParse.XmlResult (String, XmlElement, ReaderState)
readXml state@(ReaderState _ _ _ Preamble _) text = do
  (remainder, xml) <- XmlParse.parsePreamble text
  return (remainder, xml, state)

readXml state@(ReaderState _ _ _ StreamStart ctx) text = do
  (remainder, xml, ctx') <- XmlParse.parseStartTag ctx text
  return (remainder, xml, state { rdrContext = ctx' })

readXml state@(ReaderState _ _ _ Any ctx) text = do
  (remainder, xml) <- XmlParse.parseNestedTag ctx text
  return (remainder, xml, state)

{- -------------------------------------------------------------------------- -}

-- | Proceses an individual XML block, returning a new reader state value
--   depending on the actions taken
processStanza :: ConnMsgQ -> ReaderState -> XmlElement -> ReadResultIO ReaderState
processStanza q state (XmlProcessingInstruction _) =  return state
processStanza q state@(ReaderState _ _ _ expected ctx) xml = do
  (lift . debug) $ "xml: " ++ (show xml)

  let expected' = if expected == Preamble then StreamStart else expected
  let state' = state {rdrExpect = expected'}

  case Xmpp.fromXml xml of
    Just stanza -> do
      debugM $ "stanza: " ++ (show stanza)
      let rVar = rdrCondVar state
      liftIO $ atomically $ writeTChan q (InboundXmpp stanza rVar)
      debugM $ "Waiting for response..."
      rval <- (lift . takeMVar) rVar
      debugM $ "Response was " ++ (show rval)

      return $ processResponse state rval
    -- this is just an xml-to-xmpp translation failure, which shouldn't
    -- necessarily be a fatal error... just return the existing state and carry
    -- on
    Nothing -> do
      debugM $ "XMPP parse failure for XML " ++ (show xml)
      return state
  where
    processResponse :: ReaderState -> [Response] -> ReaderState
    processResponse s r = foldl' updateState s r

    updateState :: ReaderState -> Response -> ReaderState
    updateState s (Connection.Expect expected) = s {rdrExpect = expected}
    updateState s _ = s

{- -------------------------------------------------------------------------- -}

-- | Handles a message from the message queue, taking whatever actions are
-- appropriate and returning a new state  (or an error)
handleMessage :: ConnMsg -> ConnState -> ConnResultIO ConnState
handleMessage (InboundXmpp stanza sig) state =
  do
    (response, state') <- handleInboundStanza stanza state
    liftIO $ putMVar sig response
    return state'
  `catchError` \e -> do
      liftIO $ putMVar sig [Quit]
      throwError e

handleMessage m s = return s

{- -------------------------------------------------------------------------- -}

handleInboundStanza :: Xmpp.Stanza -> ConnState -> ConnResultIO ([Response], ConnState)
handleInboundStanza (Xmpp.RxStreamOpen _ _) state = do
  debugM $ "received stream open block"
  let s = stateSocket state
  liftIO $ xmppSend s $ Xmpp.TxStreamOpen "localhost" (idNumber state)
  liftIO $ xmppSend s $ (xmppFeatures (Connection.isAuthenticated state))
  return ([Ok, Connection.Expect Any], state)

handleInboundStanza (Xmpp.AuthMechanism m) state = do
  debugM $ "received authentication selection request: " ++ m
  auth <- liftIO $ newAuthInfo m "jhabber"
  case auth of
    Just a -> do
      liftIO $ xmppSend (stateSocket state) $ Xmpp.AuthChallenge (challenge a)
      let state' = state { authInfo = a }
      return ([Ok], state')
    Nothing -> throwError UnsupportedMechanism

handleInboundStanza (Xmpp.AuthResponse r) state = do
  debugM $ "received authentication response " ++ (show $ authInfo state)
  case checkResponse (authInfo state) r of
    Right (authInfo', authAction) -> do
      state' <- handleAuthResponse state authInfo' authAction
      let expected = if authAction == Success then StreamStart else Any
      return ([Ok, Connection.Expect expected], state')
    Left _ -> throwError AuthFailure
  `catchError` \e -> do
    liftIO $ debug $ "Error handling auth response: " ++ (show e)
    case e of
      AuthFailure -> do
        liftIO $ authFailure state "temporary-auth-failure"
      _ -> return ()
    throwError e

handleInboundStanza iq@(Xmpp.Iq _ _ _ _ _) state = do
  debugM $ "IQ"
  if not (Connection.isAuthenticated state)
    then throwError NotAuthenticated
    else do
      (response, state') <- handleIq iq state
      liftIO $ xmppSend (stateSocket state) response
      return ([Ok], state')

handleInboundStanza message@(Xmpp.Message to _ _ _ _ _ _ _) state = do
    case to of
      Just dst ->
        let src = maybe (stateJid state) (id) (msgFrom msg)
        routeMessage (router state) src dst $ translateMsg msg
      _ -> ()
    return ([Ok], state)
  where
    translateMsg :: Xmpp.Stanza -> InstantMessage
    translateMsg xml@(Xmpp.Message _ _ xmppType lang subject thread body _) =
      MkMsg {  msgType = xmppType,
               msgLang = lang,
               msgSubject = map repack subject
               msgThread = thread
               msgBody = map repack body }

    repack :: Xmpp.TaggedText -> TaggedText
    repack (lang, text) = (maybe Nothing (\s -> Just $ pack s) lang, pack text)

handleInboundStanza _ state = return ([Ok], state)

{- -------------------------------------------------------------------------- -}

uid :: ConnState -> String
uid s = Sasl.getUid $ authInfo s

{- -------------------------------------------------------------------------- -}

handleIq :: Xmpp.Stanza -> ConnState -> ConnResultIO (Xmpp.Stanza, ConnState)
handleIq iq@(Xmpp.Iq rid to from action (xml:_)) state = do
  response <- liftIO $ runErrorT $ runIq action xml state
  case response of
    Right (response, state') -> do
      let stanza = Xmpp.Iq rid from to Xmpp.Result response
      return (stanza, state')
    Left err ->
      return (Xmpp.Iq rid from to Xmpp.Error (showError xml err), state)
  where
    showError :: XmlElement -> ConnFailure -> [XmlElement]
    showError req error =
      let (errType, xmlName) = case error of
                                 AccessDenied -> ("cancel","not-allowed")
                                 BadRequest -> ("modify","bad-request")
                                 BindConflict -> ("cancel","conflict")
                                 Connection.NotFound -> ("cancel","item-not-found")
                                 ServiceUnavailable -> ("cancel","service-unavailable")
          xml = Xmpp.formatError errType xmlName
      in [req, xml]


{- -------------------------------------------------------------------------- -}

runIq :: Xmpp.IqAction ->  XmlElement -> ConnState -> ConnResultIO ([XmlElement], ConnState)
runIq action xml state = do
  let contentName = (Xmpp.parseNamespace (elemNamespace xml), elemName xml)
  case contentName of
    (Xmpp.NsBind, "bind") -> handleBind action xml state
    (Xmpp.NsSession, "session") -> handleSession action xml state
    (Xmpp.NsDiscovery, "query") -> handleServiceDiscovery action xml state
    (Xmpp.NsRoster, "query") -> handleRosterQuery action xml state
    _ -> throwError ServiceUnavailable

{- -------------------------------------------------------------------------- -}

handleBind :: Xmpp.IqAction -> XmlElement -> ConnState -> ConnResultIO ([XmlElement], ConnState)
handleBind action xml state = do
  case action of
    Xmpp.Set -> do
      let resourceName = Xmpp.getBindResourceName xml
      let jid = Xmpp.JID (uid state) localhost resourceName
      let handle = getConnectionHandle state
      jid' <- liftRouterIO $ bindResource (router state) (getConnectionHandle state) jid
      let state' = state { stateJid = jid' }
      return ( [XmlElement "" "jid" [] [XmlText (show jid')]], state')
    _ -> throwError ServiceUnavailable

{- -------------------------------------------------------------------------- -}

handleSession :: Xmpp.IqAction -> XmlElement -> ConnState -> ConnResultIO ([XmlElement], ConnState)
handleSession action content state = do
  case action of
    Xmpp.Set -> do
      liftRouterIO $ activateSession (router state) (stateJid state)
      return ([], state)
    _ -> throwError ServiceUnavailable

{- -------------------------------------------------------------------------- -}

handleServiceDiscovery :: Xmpp.IqAction -> XmlElement -> ConnState -> ConnResultIO([XmlElement], ConnState)
handleServiceDiscovery action content state = do
--  let jid = stateJid state
--  items <- liftRouterIO $ discoverInfo (router state) jid
  return $ ([], state)

{- -------------------------------------------------------------------------- -}

handleRosterQuery :: Xmpp.IqAction -> XmlElement -> ConnState -> ConnResultIO([XmlElement], ConnState)
handleRosterQuery action content state = do
  case action of
    Xmpp.Get -> do
      let Xmpp.JID uid _ _ = stateJid state
      debugM $ "Fetching roster for " ++ uid
      roster <- liftIO $ fetchRoster (database state) uid
      let nsAttrib = XmlAttribute "" "xmlns" (show Xmpp.NsRoster)
      let items = (Xmpp.formatRoster roster)
      return ([XmlElement "" "query" [nsAttrib] items], state)
    _ -> throwError ServiceUnavailable

{- -------------------------------------------------------------------------- -}

routeMessage :: JID -> JID -> Router -> Xmpp.Stanza -> IO ()
routeMessage to from rtr msg = do
  routeMessage rtr to from $ Message

{- -------------------------------------------------------------------------- -}

-- | Lifts a router result into the connection result monad by translating the
--   router failure code into the appropriate connection result code
liftRouterIO :: LocalRouter.ResultIO a -> ConnResultIO a
liftRouterIO = mapErrorT translateResult
  where
    translateResult :: IO (Either LocalRouter.Failure a) -> IO (Either ConnFailure a)
    translateResult rval = do
      rval' <- rval
      case rval' of
        Right x -> return $ Right x
        Left err -> return (Left $ translate err)

    translate :: LocalRouter.Failure -> ConnFailure
    translate f =
      if f == LocalRouter.Timeout
        then Connection.Timeout
        else UnknownFailure

{- -------------------------------------------------------------------------- -}

handleAuthResponse :: ConnState -> AuthInfo -> AuthResponse -> ConnResultIO ConnState
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

{- -------------------------------------------------------------------------- -}

authenticate :: ConnState -> AuthInfo -> ConnResultIO ConnState
authenticate state authInfo = do
  let uid = getUid authInfo
  info <- liftIO $ lookupUser (database state) uid
  case info of
    Nothing -> throwError AuthFailure
    Just userInfo ->
      case checkCredentials authInfo uid (userPassword userInfo) of
        Right (authInfo', Challenge rval) -> do
          liftIO $ xmppSend (stateSocket state) $ Xmpp.AuthChallenge rval
          return state { authInfo = authInfo' }

        Left _ -> throwError AuthFailure

{- -------------------------------------------------------------------------- -}

isAuthenticated :: ConnState -> Bool
isAuthenticated s = Sasl.isAuthenticated $ authInfo s

{- -------------------------------------------------------------------------- -}

getConnectionHandle :: ConnState -> Connection
getConnectionHandle state = Conn (idNumber state) (msgQ state)

{- -------------------------------------------------------------------------- -}

authFailure :: ConnState -> String -> IO ()
authFailure state reason = xmppSend (stateSocket state)  $ Xmpp.newAuthFailure reason

-- | Formats an XMPP stanza and sends it out on the socket
xmppSend :: Socket -> Xmpp.Stanza -> IO ()
xmppSend socket stanza = do
  rval <- send socket $ Xmpp.format stanza
  -- error handling goes here
  return ()

{- -------------------------------------------------------------------------- -}

xmppFeatures :: Bool -> Xmpp.Stanza
xmppFeatures authorized =
  Xmpp.Features $ mechanisms : features
  where
    features = case authorized of
      True -> [
        (XmlElement "" "bind" [XmlAttribute "" "xmlns" "urn:ietf:params:xml:ns:xmpp-bind"] []),
        (XmlElement "" "session" [XmlAttribute "" "xmlns" "urn:ietf:params:xml:ns:xmpp-session"] [])]
      False -> []
    mechanisms =
      XmlElement "" "mechanisms"
        [XmlAttribute "" "xmlns" "urn:ietf:params:xml:ns:xmpp-sasl"]
        xmppMechanisms

{- -------------------------------------------------------------------------- -}

xmppMechanisms :: [XmlElement]
xmppMechanisms = [
  (XmlElement "" "mechanism" [] [XmlText "DIGEST-MD5"]),
  (XmlElement "" "mechanism" [] [XmlText "PLAIN"]) ]

debugM s = liftIO $ debug s

debug :: String -> IO ()
debug = L.debugM "conn"
