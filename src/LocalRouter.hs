module LocalRouter (Router,
                    Failure(..),
                    ResultIO,
                    newRouter,
                    stopRouter,
                    bindResource,
                    activateSession,
                    discoverInfo,
                    crashRouter) where

import Control.Monad.Error
import Prelude hiding (catch)
import Data.List
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Concurrent.STM
import qualified Control.Exception as E
import System.Log.Logger
import System.Timeout
import Xmpp(JID(..))
import {-# SOURCE #-} Connection

import RoutingTable
import qualified Xmpp

data Failure = Timeout
             | NotFound
  deriving (Eq, Show, Read)

instance Error Failure where
  strMsg x = read x

type NamedItem = (JID, String)

data Reply = RpyBound JID
           | RpyError Failure
           | RpyItems [NamedItem]
           | RpyOK
  deriving (Eq, Show)

{-| A signal used send a response back to a waiting client -}
type ReplyVar = TMVar Reply

{-| The messages that our worker threads can handle -}
data Message = MsgDummy
             | MsgCrash
             | MsgBind Connection JID ReplyVar
             | MsgActivateSession JID ReplyVar
             | MsgQuery JID String ReplyVar

{-| The messages that the router manager thread can handle -}
data RouterMsg = Quit
               | Crashed ThreadId E.SomeException
               | Exited ThreadId
  deriving(Show)

type Result a = Either Failure a
type ResultIO a = ErrorT Failure IO a

type CommandQueue = TChan RouterMsg
type MessageQueue = TChan Message
type RoutingTableVar = TVar RoutingTable

data Router = MkRouter {
  rtrMsgQ :: MessageQueue,
  rtrCmdQ :: CommandQueue,
  rtrTable :: RoutingTableVar
}

data RouterState = State {
  stateMsgQ :: MessageQueue,
  stateCmdQ :: CommandQueue,
  stateWorkers :: [ThreadId],
  stateTable :: RoutingTableVar
}

{- -------------------------------------------------------------------------- -}

newRouter :: Int -> IO Router
newRouter nThreads = do
  msgQ <- newTChanIO -- :: (Chan Message)
  cmdQ <- newTChanIO
  t <- newTVarIO $ Map.empty
  forkIO $ routerMain nThreads cmdQ msgQ t
  return $ MkRouter { rtrMsgQ = msgQ, rtrCmdQ = cmdQ, rtrTable = t }

{- -------------------------------------------------------------------------- -}

stopRouter :: Router -> IO ()
stopRouter r = do
  debug "Stopping router"
  atomically $ writeTChan (rtrCmdQ r) Quit
  threadDelay 1000000
  return ()

{- -------------------------------------------------------------------------- -}

newReplyVar :: IO ReplyVar
newReplyVar = newEmptyTMVarIO

{- -------------------------------------------------------------------------- -}

-- | todo: handle the case where the resource is already bound to to another
--         connection
bindResource :: Router -> Connection -> JID -> ResultIO JID
bindResource r conn jid = do
  replyVar <- liftIO $ newReplyVar
  reply <- sendMessageAndWait r (MsgBind conn jid replyVar) replyVar 1000
  case reply of
    RpyBound jid' -> return jid'

{- -------------------------------------------------------------------------- -}

activateSession :: Router -> JID -> ResultIO ()
activateSession r jid = do
  replyVar <- liftIO $ newReplyVar
  reply <- sendMessageAndWait r (MsgActivateSession jid replyVar) replyVar 1000
  case reply of
    RpyOK -> return ()

discoverInfo :: Router -> JID -> String -> ResultIO [NamedItem]
discoverInfo r jid dst = do
  replyVar <- liftIO newReplyVar
  reply <- sendMessageAndWait r (MsgQuery jid dst replyVar) replyVar 1000
  case reply of
    RpyItems ns -> return ns

{- -------------------------------------------------------------------------- -}

sendMessageAndWait :: Router -> Message -> ReplyVar -> Int -> ResultIO Reply
sendMessageAndWait rtr msg rpyVar t = do
  let q = rtrMsgQ rtr
  liftIO (atomically $ writeTChan q msg)
  maybeReply <- liftIO $ timeout t (atomically $ takeTMVar rpyVar)
  case maybeReply of
    Just rpy -> return rpy
    Nothing -> throwError Timeout

{- -------------------------------------------------------------------------- -}

crashRouter :: Router -> IO ()
crashRouter r = do
  atomically $ writeTChan (rtrMsgQ r) MsgCrash
  return ()

{- -------------------------------------------------------------------------- -}

routerMain :: Int -> CommandQueue -> MessageQueue -> RoutingTableVar -> IO ()
routerMain nThreads cmdQ msgQ rTable = do
  debug "Starting new local router thread"
  let router = MkRouter msgQ cmdQ rTable
  ts <- mapM (\_ -> forkWorker router) [1..nThreads]
  loop State { stateCmdQ = cmdQ,
               stateMsgQ = msgQ,
               stateWorkers = ts,
               stateTable = rTable }
    where
      loop :: RouterState -> IO ()
      loop state = do cmd <- atomically $ readTChan cmdQ
                      result <- handleCmd cmd state
                      case result of
                        Just state' -> loop state'
                        Nothing -> return ()

{- -------------------------------------------------------------------------- -}

handleCmd :: RouterMsg -> RouterState -> IO (Maybe RouterState)
handleCmd Quit s = do
  debug "Quitting manager thread"
  return Nothing

handleCmd (Crashed tid _) s = do
  debug $ "Worker thread " ++ (show tid) ++ " crashed!"
  let workers = delete tid (stateWorkers s)
  let router = MkRouter (stateMsgQ s) (stateCmdQ s) (stateTable s)
  tid <- forkWorker router
  return (Just s { stateWorkers = tid : workers})

hanldleCmd _ s = do
  return (Just s)

{- -------------------------------------------------------------------------- -}

forkWorker :: Router -> IO ThreadId
forkWorker router = do
  forkIO $ do
    tid <- myThreadId
    debug $ "Starting new router worker on: " ++ (show tid)
    result <- tryWorkerThread router
    atomically $ writeTChan (rtrCmdQ router) $
      either (Crashed tid) (const (Exited tid)) result

{- -------------------------------------------------------------------------- -}

tryWorkerThread :: Router -> IO (Either E.SomeException ())
tryWorkerThread router = do
  E.try $ workerThread router

{- -------------------------------------------------------------------------- -}

workerThread :: Router  -> IO ()
workerThread router = loop router
  where
    loop :: Router -> IO ()
    loop r = do
      msg <- atomically $ readTChan (rtrMsgQ r)
      handleMsg r msg
      loop r

{-| Handles a worker thread message -}
handleMsg :: Router -> Message -> IO ()

-- Binds a connection to a jabber ID in the routing table
handleMsg r (MsgBind conn jid replyVar) = do
  let res = newResource jid conn
  atomically $ do
    table <- readTVar (rtrTable r)
    let reg    = maybe (newRegistration) id (lookupRegistration jid table)
        table' = updateResource jid reg res table
    writeTVar (rtrTable r) $! table'
  atomically $ putTMVar replyVar $! RpyBound jid

-- Activates a session on a bound resource
handleMsg r (MsgActivateSession jid replyVar) = do
  reply <- atomically $ do
    table <- readTVar (rtrTable r)
    let maybeRes = lookupResource jid table
    case maybeRes of
      Just (reg,res) -> do
        let res' = res { resActive = True }
            table' = updateResource jid reg res' table
        writeTVar (rtrTable r) $! table'
        return RpyOK
      Nothing -> return $ RpyError NotFound
  atomically $ putTMVar replyVar $! reply

-- Handles a discovery query from a client. Only ever returns an empty set of
-- name/value pairs
handleMsg r (MsgQuery jid dst replyVar) = replyTo replyVar (RpyItems [])

-- Crash is for debugging purposes only
handleMsg r MsgCrash = do
  tid <- myThreadId
  debug $ "crash requested on " ++ (show tid)
 -- let i = 1 / 0
 -- debug $ "div by 0 " ++ (show i)
  E.throwIO E.DivideByZero
  debug $ (show tid) ++ " should be crashed"
  return ()

{- -------------------------------------------------------------------------- -}

replyTo :: ReplyVar -> Reply -> IO ()
replyTo replyVar reply = atomically $ putTMVar replyVar $! reply

{- -------------------------------------------------------------------------- -}

debug = debugM "localRouter"
