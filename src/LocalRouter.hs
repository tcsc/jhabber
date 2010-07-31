module LocalRouter (Router,
                    newRouter,
                    stopRouter,
                    bindResource,
                    crashRouter) where

import Prelude hiding (catch)
import Data.List
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Concurrent.STM
import qualified Control.Exception as E
import System.Log.Logger
import Xmpp(JID(..))
import {-# SOURCE #-} Connection

data Message = MsgDummy
             | MsgCrash

data RouterMsg = Quit
               | Crashed ThreadId E.SomeException
               | Exited ThreadId
  deriving(Show)

type CommandQueue = TChan RouterMsg
type MessageQueue = TChan Message
type Registration = Map.Map String Connection
type RoutingTable = Map.Map String Registration
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

-- | todo: handle the case where the resource is already bound to to another
--         connection
bindResource :: Router -> Connection -> JID -> IO ()
bindResource r conn jid@(JID node host resource) = do
  atomically $ do
    table <- readTVar $ rtrTable r
    
    let reg    = getRegistration jid table
        reg'   = Map.insert resource conn reg
        table' = Map.insert node reg' table
        
    writeTVar (rtrTable r) $! table'
  return ()

{- -------------------------------------------------------------------------- -}
  
getRegistration :: JID -> RoutingTable -> Registration
getRegistration jid@(JID node _ _) table = 
  case Map.lookup node table of
    Just reg -> reg
    Nothing -> Map.empty

{- -------------------------------------------------------------------------- -}

crashRouter :: Router -> IO ()
crashRouter r = do
  atomically $ writeTChan (rtrMsgQ r) MsgCrash
  return ()

{- -------------------------------------------------------------------------- -}

routerMain :: Int -> CommandQueue -> MessageQueue -> RoutingTableVar -> IO ()
routerMain nThreads cmdQ msgQ rTable = do
  debug "Starting new local router thread"
  ts <- mapM (\_ -> forkWorker cmdQ msgQ) [1..nThreads]
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
  tid <- forkWorker (stateCmdQ s) (stateMsgQ s)
  return (Just s { stateWorkers = tid : workers})

hanldleCmd _ s = do
  return (Just s)

{- -------------------------------------------------------------------------- -}

forkWorker :: CommandQueue -> MessageQueue -> IO ThreadId
forkWorker cmdQ msgQ = do
  forkIO $ do
    tid <- myThreadId
    debug $ "Starting new router worker on: " ++ (show tid)
    result <- tryWorkerThread cmdQ msgQ
    atomically $
      writeTChan cmdQ $ either (Crashed tid) (const (Exited tid)) result

{- -------------------------------------------------------------------------- -}

tryWorkerThread :: CommandQueue -> MessageQueue -> IO (Either E.SomeException ())
tryWorkerThread qCtrl qMsg = do
  E.try $ workerThread qCtrl qMsg

{- -------------------------------------------------------------------------- -}

workerThread :: CommandQueue -> MessageQueue -> IO ()
workerThread qCtrl qMsg = do
  loop qCtrl qMsg
    where loop qCtrl qMsg = do msg <- atomically $ readTChan qMsg
                               handleMsg msg
                               loop qCtrl qMsg

{- Crash is for debugging purposes only -}
handleMsg :: Message -> IO ()
handleMsg MsgCrash = do
  tid <- myThreadId
  debug $ "crash requested on " ++ (show tid)
 -- let i = 1 / 0
 -- debug $ "div by 0 " ++ (show i)
  E.throwIO E.DivideByZero
  debug $ (show tid) ++ " should be crashed"
  return ()

debug = debugM "localRouter"
