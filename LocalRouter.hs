module LocalRouter (Router, 
                    newRouter,
                    stopRouter,
                    crashRouter) where

import Prelude hiding (catch)
import Data.List
import Control.Monad
import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Concurrent.STM
import qualified Control.Exception as E
import System.Log.Logger

data Message = MsgDummy
             | MsgCrash

data RouterMsg = Quit
               | Crashed ThreadId E.SomeException
               | Exited ThreadId
  deriving(Show)

type CommandQueue = TChan RouterMsg 
type MessageQueue = TChan Message

data Router = MkRouter {  
  rtrMsgQ :: MessageQueue,
  rtrCmdQ :: CommandQueue,
  rtrThread :: ThreadId
}
  
data RouterState = State {
  stateMsgQ :: MessageQueue,
  stateCmdQ :: CommandQueue,
  stateWorkers :: [ThreadId]
}

newRouter :: Int -> IO Router
newRouter nThreads = do
  msgQ <- newTChanIO -- :: (Chan Message)
  cmdQ <- newTChanIO
  t <- forkIO $ routerMain nThreads cmdQ msgQ
  return $ MkRouter { rtrMsgQ = msgQ, rtrCmdQ = cmdQ, rtrThread = t }
  
stopRouter :: Router -> IO ()
stopRouter r = do
  debug "Stopping router"
  atomically $ writeTChan (rtrCmdQ r) Quit
  threadDelay 1000000
  return ()
  
crashRouter :: Router -> IO ()
crashRouter r = do
  atomically $ writeTChan (rtrMsgQ r) MsgCrash
  return ()
  
routerMain :: Int -> CommandQueue -> MessageQueue -> IO ()
routerMain nThreads cmdQ msgQ = do
  debug "Starting new local router thread"
  ts <- mapM (\_ -> forkWorker cmdQ msgQ) [1..nThreads]
  loop State { stateCmdQ = cmdQ, stateMsgQ = msgQ, stateWorkers = ts  }
    where
      loop :: RouterState -> IO () 
      loop state = do cmd <- atomically $ readTChan cmdQ
                      result <- handleCmd cmd state
                      case result of 
                        Just state' -> loop state'
                        Nothing -> return ()
 
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
  
forkWorker :: CommandQueue -> MessageQueue -> IO ThreadId
forkWorker cmdQ msgQ = do
  forkIO $ do 
    tid <- myThreadId
    debug $ "Starting new router worker on: " ++ (show tid) 
    result <- tryWorkerThread cmdQ msgQ
    atomically $ 
      writeTChan cmdQ $ either (Crashed tid) (const (Exited tid)) result

tryWorkerThread :: CommandQueue -> MessageQueue -> IO (Either E.SomeException ())
tryWorkerThread qCtrl qMsg = do
  E.try $ workerThread qCtrl qMsg
  
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