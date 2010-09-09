module WorkerPool(WorkerPool,
                  WorkerSetup,
                  WorkerTeardown,
                  MessageHandler,
                  call,
                  callWithTimeout,
                  post,
                  newWorkerPool,
                  stopWorkerPool) where

import Control.Exception(SomeException, bracket, try)
import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Concurrent.STM
import Data.List
import Data.Maybe
import System.Timeout(timeout)
import System.Log.Logger

-- | Defines an "operation complete" signal as well as providing a mechanism
--   where a message handler can return a result to the original caler
type ReplyVar reply = TMVar reply

-- | Defines a set of messages used for iternal communication between the
--   Manager thread and the worker threads
data MgrMsg = Exit
            | ThreadExit ThreadId (Maybe SomeException)

-- | Defines a set of messages that the worker threads can process
data WkrMsg msg reply = ExitWorker
                      | Handle msg
                      | HandleAndReply msg (ReplyVar reply)

type MgrQ = TChan MgrMsg
type WkrQ msg reply = TChan (WkrMsg msg reply)

-- | Defines a function that sets up a worker thread. Use this to initialise any
--   thread - specific data (e.g. database handles, etc) if necessary
type WorkerSetup state = IO state

-- | Defines a function for tearing down resources created during thread startup
type WorkerTeardown state = state -> IO ()

-- | Defines a function type that the worker pool uses to handle messages
type MessageHandler msg   -- ^ The range of possible messages
                    reply -- ^ The range of possible replies
                    state -- ^ An opaque per-thread state block for the owner
                    = msg -> state -> IO ((Maybe reply), state)

data PoolFuns msg reply threadState =
  PoolFuns (WorkerSetup threadState)
           (WorkerTeardown threadState)
           (MessageHandler msg reply threadState)

data WorkerPool msg reply = MkWorkerPool MgrQ (WkrQ msg reply)

data PoolState msg reply state = PoolState { poolThreadCount :: Int,
                                             poolThreads :: [ThreadId],
                                             poolFuns :: PoolFuns msg reply state }

data WorkerPoolError = Timeout

type WorkerResultIO reply = IO (Either WorkerPoolError reply)

-- | Forks off a pool manager thread and returns a handle to the newly created
--   worker pool
newWorkerPool :: Int ->
                 WorkerSetup state ->              -- ^ creates a new worker thread state
                 WorkerTeardown state ->           -- ^ destroys a worker thread state
                 MessageHandler msg reply state -> -- ^ the message handling function
                 IO (WorkerPool msg reply)
newWorkerPool threads setup teardown handler = do
  mgrQ <- newTChanIO
  wkrQ <- newTChanIO
  let funs = PoolFuns setup teardown handler
  forkIO $ mgrThreadMain threads mgrQ wkrQ funs
  return $ MkWorkerPool mgrQ wkrQ

-- | Stops the worker pool, but does not wait for the pool threads to exit.
stopWorkerPool :: WorkerPool msg reply -> IO ()
stopWorkerPool (MkWorkerPool mgrQ _) = do
  atomically $ writeTChan mgrQ Exit

-- | The main routine for the pool manager thread. Starts and stops the worker
--   threads as well as monitring the management queue for messages and acts on
--   them.
mgrThreadMain :: Int -> MgrQ -> WkrQ msg reply -> PoolFuns msg reply state -> IO ()
mgrThreadMain nThreads mgrQ wkrQ handlers = do
    ts <- mapM (\_ -> forkWorker mgrQ wkrQ handlers ) [1..nThreads]
    loop mgrQ wkrQ $ PoolState { poolThreadCount = nThreads,
                                 poolThreads = ts,
                                 poolFuns = handlers }
  where
    loop :: MgrQ -> WkrQ msg reply -> PoolState msg reply state -> IO ()
    loop mgrQ wkrQ state = do
      msg <- atomically $ readTChan mgrQ
      case msg of
        Exit -> do
          atomically $ mapM (\_ -> writeTChan wkrQ ExitWorker) [1..nThreads]
          loop mgrQ wkrQ state

        ThreadExit tid maybeError -> do
          let tids = delete tid (poolThreads state)
          case maybeError of
            -- The thread has exited normally, probably as part of a controlled
            -- shutdown. Let it go.
            Nothing -> loop mgrQ wkrQ $ state { poolThreads = tids }

            -- The thread has crashed. Restart it and hope that whatever caused
            -- the crash has gone away.
            Just e -> do
              logError $ (show tid) ++ " crashed with " ++ (show e)
              newTid <- forkWorker mgrQ wkrQ (poolFuns state)
              loop mgrQ wkrQ $ state { poolThreads = newTid : tids }

        -- Unknown message. Ignore.
        _ -> loop mgrQ wkrQ state

-- | Forks off a worker thread that will handle messages from the worker queue
--   and process them, posting a message back to the manager thread when it
--   exits for whatever reason
forkWorker :: MgrQ -> WkrQ msg reply -> PoolFuns msg reply state -> IO ThreadId
forkWorker mgrQ wkrQ funs = forkIO $ tryWorker mgrQ wkrQ funs
  where
    tryWorker :: MgrQ -> WkrQ msg reply-> PoolFuns msg reply state -> IO ()
    tryWorker mgrQ wkrQ (PoolFuns setup teardown handler) = do
      tid <- myThreadId
      result <- try $ bracket setup teardown (loop mgrQ wkrQ handler)
      let response = either (Just) (\_ -> Nothing) result
      atomically $ writeTChan mgrQ $ ThreadExit tid response

    loop :: MgrQ -> WkrQ msg reply-> MessageHandler msg reply state -> state -> IO ()
    loop mgrQ wkrQ handler state = do
      msg <- atomically $ readTChan wkrQ
      case msg of
        Handle m -> do
          (_, state') <- handler m state
          loop mgrQ wkrQ handler state'
        HandleAndReply m r -> do
          (reply, state') <- handler m state
          sendReply r reply
          loop mgrQ wkrQ handler state'

-- | Posts a message to the worker pool and awaits a reply
call :: WorkerPool msg reply -> msg -> IO reply
call (MkWorkerPool _ wkrQ) msg = do
  replyVar <- newReplyVar
  atomically $ writeTChan wkrQ $! HandleAndReply msg replyVar
  atomically $ takeTMVar replyVar

-- | Posts a message to the worker queue and waits - up to a given timeout - for
--   a reply. Returns Nothing if the request times out.
callWithTimeout :: WorkerPool msg reply -> msg -> Int -> WorkerResultIO reply
callWithTimeout (MkWorkerPool _ wkrQ) msg t = do
  replyVar <- newReplyVar
  atomically $ writeTChan wkrQ $! HandleAndReply msg replyVar
  result <- timeout t $ atomically $ takeTMVar replyVar
  case result of
    Just rpy -> return $ Right rpy
    Nothing -> return $ Left Timeout

-- | Posts a message to the worker pool and does not await a reply
post :: WorkerPool msg reply -> msg -> IO ()
post (MkWorkerPool _ wkrQ) msg = atomically $
  writeTChan wkrQ $! Handle msg

-- | Forces the evaluation of the reply and sends it back to the caller via
--   the supplied ReplyVar
sendReply :: ReplyVar reply -> Maybe reply -> IO ()
sendReply replyVar reply = atomically $ putTMVar replyVar $! fromJust reply

newReplyVar :: IO (ReplyVar reply)
newReplyVar = newEmptyTMVarIO

debug = debugM "pool"
logError = errorM "pool"
