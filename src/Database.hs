module Database(JhabberDb,
                UserInfo(..),
                startDb,
                stopDb,
                createAccount,
                lookupUser ) where

import Control.Concurrent
import Control.Exception(SomeException, Exception, try, bracket, throwIO)
import Control.Monad.Reader
import Database.MongoDB
import System.Log.Logger
import Data.Typeable
import Data.Bson
import Data.UString(UString, u)
import qualified Data.List as L
import WorkerPool
import Xmpp

data DbError = DbError Database.MongoDB.Failure
             deriving (Show, Typeable)

instance Exception DbError

data UserInfo = UserInfo { userName :: !String,
                           userPassword :: !String }
                deriving (Eq, Show, Typeable)

data DbMsg = MsgCreateAccount !String !String
           | MsgLookupAccount !String
           | MsgFetchRoster !String
           deriving (Show)

data DbReply = RpyOK
             | RpyUserInfo (Maybe UserInfo)
             | RpyFail SomeException
             deriving  (Show)

data WorkerState = WkrState Connection UString

data JhabberDb = MkDatabase (WorkerPool DbMsg DbReply)

-- | Connects to a database
startDb :: String -> String -> IO JhabberDb
startDb host dbName = do
  pool <- newWorkerPool 4 (openConnection host dbName) (closeConnection) (handleMessage)
  return $ MkDatabase pool

-- | Stops the database and closes all connections to it
stopDb :: JhabberDb -> IO ()
stopDb (MkDatabase workerPool) = stopWorkerPool workerPool

openConnection :: String -> String -> IO WorkerState
openConnection server db = do
  tid <- myThreadId
  debugM "db" $ "opening conn on thread " ++ (show tid)
  rval <- runNet $ connect (host server)
  case rval of
    Right conn -> return $ WkrState conn (u db)
    Left err -> throwIO err

closeConnection :: WorkerState -> IO ()
closeConnection (WkrState conn _) = do
  tid <- myThreadId
  debugM "db" $ "closing conn on thread " ++ (show tid)
  close conn

-- | Creates account
createAccount :: JhabberDb -> String -> String -> IO Bool
createAccount (MkDatabase pool) uid pwd = do
  result <- call pool $ MsgCreateAccount uid pwd
  case result of
    RpyOK -> return True
    RpyFail _ -> return False

-- | Looks up a given user based on their account name
lookupUser :: JhabberDb -> String -> IO (Maybe UserInfo)
lookupUser (MkDatabase pool) userName = do
  result <- call pool $ MsgLookupAccount userName
  case result of
    RpyUserInfo maybeUser -> return maybeUser
    RpyFail _ -> return Nothing

handleMessage :: DbMsg -> WorkerState -> IO (Maybe DbReply, WorkerState)
handleMessage (MsgCreateAccount uid pwd) state@(WkrState conn _) = return (Just RpyOK, state)

{- Looks up a user based on their username -}
handleMessage (MsgLookupAccount uid) state@(WkrState conn db) = do
  rval <- runNet $ runConn (findUser db $ u uid) conn
  userInfo <- unpackResult rval
  debugL $ "lookup returned " ++ (show userInfo)
  return (Just $ RpyUserInfo userInfo, state)

unpackResult :: Either IOError (Either Database.MongoDB.Failure a) -> IO a
unpackResult result = case result of
                        Left err -> throwIO err
                        Right dbResponse -> case dbResponse of
                                              Right x -> return x
                                              Left err -> throwIO (DbError err)

-- findUser -- types too complicated to worry about
findUser db uid =  useDb db $ do
  doc <- findOne (select [userid =: uid] (u"users"))
  case doc of
    Just d -> return $ readUser d
    Nothing -> return Nothing

readUser :: Document -> Maybe UserInfo
readUser doc = do
  uid <- Data.Bson.lookup (u"userid") doc
  pwd <- Data.Bson.lookup (u"password") doc
  return (UserInfo uid pwd)

instance Val UserInfo where
  val (UserInfo uid pwd) = Doc [userid =: (u uid), password =: (u pwd)]
  cast' (Doc doc) = readUser doc
  cast' _ = Nothing

userid :: Label
userid = u "userid"
password =  u"password"

{-
instance JSON UserInfo where
  readJSON (JSObject obj) = let objs = fromJSObject obj in do
    userid <- mLookup "userid" objs >>= readJSON
    password <- mLookup "password" objs >>= readJSON
    return (UserInfo userid password)
  readJSON _ = fail ""

  showJSON (UserInfo uid pwd) = makeObj [("_id", showJSON uid),
                                         ("userid", showJSON uid),
                                         ("password", showJSON pwd)]

instance JSON RosterEntry where
  readJSON (JSObject obj) = let objs = fromJSObject obj in do
    owner <- mLookup "owner" objs >>= readJSON
    jidText <- mLookup "target" objs >>= readJSON
    jid <- fromMaybe $ parseJid jidText
    name <- mLookup "name" objs >>= readJSON
    return $ RosterEntry owner jid name []
  readJSON _ = fail ""

  showJSON (RosterEntry owner jid name groups) = makeObj [("owner", showJSON owner),
                                                          ("target", showJSON $ show jid),
                                                          ("name", showJSON name),
                                                          ("groups", showJSON groups)]

instance JSON RosterGroup where
  readJSON (JSString s) = return $ (RosterGroup $ fromJSString s)
  readJSON _ = fail ""
  showJSON (RosterGroup group) = showJSON group


userDb = db "jhusers"
rosterDb = db "jhrosters"

fromMaybe m = maybe  (fail "") (return) m
mLookup a as = maybe (fail $ "no such element: " ++ a) (return) $ L.lookup a as
-}

debugL s = liftIO $ debug s
debug = debugM "db"
error = errorM "db"

