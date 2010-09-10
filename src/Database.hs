module Database(JhabberDb,
                UserInfo(..),
                startDb,
                stopDb,
                createAccount,
                lookupUser,
                fetchRoster ) where

import Control.Concurrent
import Control.Exception(SomeException, Exception, try, bracket, throwIO)
import Control.Monad.Reader
import Database.MongoDB
import System.Log.Logger
import Data.Typeable
import Data.Bson
import Data.Maybe
import Data.UString(UString, u, unpack)
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
             | RpyUserInfo !(Maybe UserInfo)
             | RpyRoster ![RosterEntry]
             | RpyFail !SomeException
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
  debugM "db" $ "opening conn on " ++ (show tid)
  rval <- runNet $ connect (host server)
  case rval of
    Right conn -> return $ WkrState conn (u db)
    Left err -> throwIO err

closeConnection :: WorkerState -> IO ()
closeConnection (WkrState conn _) = do
  tid <- myThreadId
  debugM "db" $ "closing conn on " ++ (show tid)
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

fetchRoster :: JhabberDb -> String -> IO [RosterEntry]
fetchRoster (MkDatabase pool) userName = do
  result <- call pool $ MsgFetchRoster userName
  case result of
    RpyRoster roster -> return roster
    RpyFail _ -> return []

handleMessage :: DbMsg -> WorkerState -> IO (Maybe DbReply, WorkerState)
handleMessage (MsgCreateAccount uid pwd) state@(WkrState conn _) = return (Just RpyOK, state)

{- Looks up a user based on their username -}
handleMessage (MsgLookupAccount uid) state@(WkrState conn db) = do
  userInfo <- runQuery db conn $ findUser (u uid)
  debugL $ "lookup returned " ++ (show userInfo)
  return (Just $ RpyUserInfo userInfo, state)

handleMessage (MsgFetchRoster uid) state@(WkrState conn db) = do
  roster <- runQuery db conn $ fetchUserRoster (u uid)
  return (Just $ RpyRoster roster, state)

-- runQuery :: Database -> Connection -> ReaderT Database m a -> IO a
runQuery db conn query = do
  rval <- runNet $ runConn (useDb db $ query) conn
  result <- unpackResult rval
  return result

unpackResult :: Either IOError (Either Database.MongoDB.Failure a) -> IO a
unpackResult result = case result of
                        Left err -> throwIO err
                        Right dbResponse -> case dbResponse of
                                              Right x -> return x
                                              Left err -> throwIO (DbError err)

-- findUser -- types too complicated to worry about
findUser uid = do
  doc <- findOne (select [userid =: uid] users)
  case doc of
    Just d -> return $ readUser d
    Nothing -> return Nothing

fetchUserRoster uid = do
  entries <- find (select [owner =: uid] roster) >>= rest
  return $ map (\d -> fromJust $ readRosterEntry d) entries

readUser :: Document -> Maybe UserInfo
readUser doc = do
  uid <- Data.Bson.lookup (u"userid") doc
  pwd <- Data.Bson.lookup (u"password") doc
  return (UserInfo uid pwd)

readRosterEntry :: Document -> Maybe RosterEntry
readRosterEntry doc = do
    user <- Data.Bson.lookup owner doc
    jid <- Data.Bson.lookup target doc
    n <- Data.Bson.lookup name doc
    grps <- Data.Bson.lookup groups doc
    return (RosterEntry user jid n grps)

instance Val UserInfo where
  val (UserInfo uid pwd) = Doc [userid =: (u uid), password =: (u pwd)]
  cast' (Doc doc) = readUser doc
  cast' _ = Nothing

instance Val RosterEntry where
  val (RosterEntry user jid n gs) = Doc [ owner =: (u user),
                                          target =: jid,
                                          name =: (u n),
                                          groups =: gs ]
  cast' (Doc d) = readRosterEntry d
  cast' _ = Nothing

instance Val JID where
  val jid = String $ u (show jid)
  cast' (String s) = parseJid $ unpack s
  cast' _ = Nothing

instance Val RosterGroup where
  val (RosterGroup s) = String $ u s
  cast' (String us) = Just $ RosterGroup (unpack us)
  cast' _ = Nothing

userid :: Label
userid = u "userid"
password =  u"password"
owner = u "owner"
target = u "target"
name = u "name"
groups = u "groups"
users = u "users"
roster = u "roster"

debugL s = liftIO $ debug s
debug = debugM "db"
error = errorM "db"

