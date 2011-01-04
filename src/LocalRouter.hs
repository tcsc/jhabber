module LocalRouter (Router,
                    Failure(..),
                    ResultIO,
                    InstantMessage(..),
                    newRouter,
                    stopRouter,
                    bindResource,
                    activateSession,
                    discoverInfo,
                    crashRouter) where

import Control.Monad.Error
import Prelude hiding (catch)
import Data.List
import Data.Text(Text, pack)
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Concurrent.STM
import qualified Control.Exception as E
import System.Log.Logger
import System.Timeout
import Xmpp(JID(..), MessageType)
import {-# SOURCE #-} Connection

import RoutingTable
import qualified Xmpp
import WorkerPool

data Failure = Timeout
             | NotFound
  deriving (Eq, Show, Read)

instance Error Failure where
  strMsg x = read x

type NamedItem = (JID, String)

{-| A string with an optional language tag -}
type TaggedText = (Maybe Text, Text)

data InstantMessage = MkMsg {
  msgType :: Xmpp.MessageType,
  msgLang :: Maybe String,
  msgSubject :: [TaggedText],
  msgThread :: Maybe String,
  msgBody :: [TaggedText]
}

data Reply = RpyBound JID
           | RpyError Failure
           | RpyItems [NamedItem]
           | RpyOK
  deriving (Eq, Show)

{-| The messages that our worker threads can handle -}
data Message = MsgDummy
             | MsgCrash
             | MsgBind Connection JID
             | MsgActivateSession JID
             | MsgQuery JID String
             | MsgRouteMessage JID JID InstantMessage

type Result a = Either Failure a
type ResultIO a = ErrorT Failure IO a

type RoutingTableVar = TVar RoutingTable

data Router = MkRouter (WorkerPool Message Reply)

{- -------------------------------------------------------------------------- -}

newRouter :: Int -> IO Router
newRouter nThreads = do
    rtVal <- newTVarIO newRoutingTable
    pool <- newWorkerPool nThreads (setup rtVal) teardown handleMsg
    return $ MkRouter pool
  where
    setup :: RoutingTableVar -> IO (RoutingTableVar)
    setup v = return v

    teardown :: RoutingTableVar -> IO ()
    teardown _ = return ()

{- -------------------------------------------------------------------------- -}

stopRouter :: Router -> IO ()
stopRouter (MkRouter workerPool) = do
  debug "Stopping router"
  stopWorkerPool workerPool

{- -------------------------------------------------------------------------- -}

-- | todo: handle the case where the resource is already bound to to another
--         connection
bindResource :: Router -> Connection -> JID -> ResultIO JID
bindResource (MkRouter workerPool) conn jid = do
  reply <- liftIO $ call workerPool (MsgBind conn jid)
  case reply of
    RpyBound jid' -> return jid'

{- -------------------------------------------------------------------------- -}

activateSession :: Router -> JID -> ResultIO ()
activateSession (MkRouter workerPool) jid = do
  liftIO $ call workerPool $ (MsgActivateSession jid)
  return ()

{- -------------------------------------------------------------------------- -}

discoverInfo :: Router -> JID -> String -> ResultIO [NamedItem]
discoverInfo (MkRouter workerPool) jid dst = do
  liftIO $ call workerPool (MsgQuery jid dst)
  return []

{- -------------------------------------------------------------------------- -}

crashRouter :: Router -> IO ()
crashRouter (MkRouter workerPool) = post workerPool MsgCrash

{- -------------------------------------------------------------------------- -}

routeMessage :: Router -> JID -> JID -> InstantMessage -> ResultIO ()
routeMessage (MkRouter workerPool) from to message =
  liftIO $ call workerPool (MsgRouteMessage from to message)
  return ()

{- -------------------------------------------------------------------------- -}

-- Binds a connection to a jabber ID in the routing table
handleMsg :: Message -> RoutingTableVar -> IO ((Maybe Reply), RoutingTableVar)
handleMsg (MsgBind conn jid) tableVar = do
  let res = newResource jid conn
  atomically $ do
    table <- readTVar tableVar
    let reg    = maybe (newRegistration) id (lookupRegistration jid table)
        table' = updateResource jid reg res table
    writeTVar tableVar $! table'
  return (Just(RpyBound jid), tableVar)

-- Activates a session on a bound resource
handleMsg (MsgActivateSession jid) tableVar = do
  reply <- atomically $ do
    table <- readTVar tableVar
    let maybeRes = lookupResource jid table
    case maybeRes of
      Just (reg,res) -> do
        let res' = res { resActive = True }
            table' = updateResource jid reg res' table
        writeTVar tableVar $! table'
        return RpyOK
      Nothing -> return $ RpyError NotFound
  return ((Just reply), tableVar)

handleMsg (MsgRouteMessage from to msg) tableVar = do
  maybeDst <- atomically $ do table <- readTVar tableVar
                              lookupResource to table
  case maybeDst of
    Just (_,res) -> (resConn res)


-- Handles a discovery query from a client. Only ever returns an empty set of
-- name/value pairs
handleMsg (MsgQuery jid dst) tableVar = return (Just (RpyItems []), tableVar)

-- Crash is for debugging purposes only
handleMsg MsgCrash tableVar = do
  tid <- myThreadId
  debug $ "crash requested on " ++ (show tid)
  E.throwIO E.DivideByZero
  debug $ (show tid) ++ " should be crashed"
  return (Nothing, tableVar)

{- -------------------------------------------------------------------------- -}

debug = debugM "localRouter"
