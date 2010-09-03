module Database(Database,
                UserInfo(..),
                startDb,
                stopDb,
                createAccount,
                lookupUser ) where

import Control.Concurrent
import Control.Exception(SomeException, try, bracket)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Log.Logger

import WorkerPool

data UserInfo = UserInfo { userID :: !Int,
                           userName :: !String,
                           userPassword :: !String }
                deriving (Show)

data DbMsg = MsgCreateAccount !String !String
           | MsgLookupAccount !String
           deriving ( Show)

data DbReply = RpyOK
             | RpyUserInfo (Maybe UserInfo)
             | RpyFail SomeException
             deriving  (Show)

data Database = MkDatabase (WorkerPool DbMsg DbReply)

-- | Connects to a database
startDb :: String -> IO Database
startDb filename = do
  pool <- newWorkerPool 4 (openConnection filename) (handleMessage) (closeConnection)
  return $ MkDatabase pool

-- | Stops the database and closes all connections to it
stopDb :: Database -> IO ()
stopDb (MkDatabase workerPool) = stopWorkerPool workerPool

openConnection filename = do
  tid <- myThreadId
  debugM "db" $ "opening conn on thread " ++ (show tid)
  connectSqlite3 filename

closeConnection :: Connection -> IO ()
closeConnection conn = do
  tid <- myThreadId
  debugM "db" $ "closing conn on thread " ++ (show tid)
  disconnect conn

-- | Creates account
createAccount :: Database -> String -> String -> IO Bool
createAccount (MkDatabase pool) uid pwd = do
  result <- call pool $ MsgCreateAccount uid pwd
  case result of
    RpyOK -> return True
    RpyFail _ -> return False

-- | Looks up a given user based on their account name
lookupUser :: Database -> String -> IO (Maybe UserInfo)
lookupUser (MkDatabase pool) userName = do
  result <- call pool $ MsgLookupAccount userName
  case result of
    RpyUserInfo maybeUser -> return maybeUser

handleMessage :: DbMsg -> Connection -> IO (Maybe DbReply, Connection)
handleMessage (MsgCreateAccount uid pwd) conn = do
  result <- try $ do run conn create_account_query [toSql uid, toSql pwd]
                     commit conn
  case result of
    Right _ -> return (Just RpyOK, conn)
    Left err -> return (Just $ RpyFail err, conn)

{- Looks up a user based on their username -}
handleMessage (MsgLookupAccount uid) conn = do
    row <- lookup_account_query conn uid
    return (Just $ RpyUserInfo (loadUser row), conn)
  where
    loadUser :: [SqlValue] -> Maybe UserInfo
    loadUser row = case row of
       [] -> Nothing
       [id, name, password] -> Just $ UserInfo (fromSql id)
                                               (fromSql name)
                                               (fromSql password)

createDatabase :: String -> IO ()
createDatabase fileName = do
  bracket (connectSqlite3 fileName) (disconnect) $
    \conn -> do
      run conn create_account_table []
      commit conn

create_account_table :: String
create_account_table = "CREATE TABLE accounts (id INTEGER PRIMARY KEY ASC, " ++
                                               "username TEXT(32) NOT NULL CONSTRAINT account_uid UNIQUE, " ++
                                               "password TEXT(32) NOT NULL) "

create_account_query :: String
create_account_query = "INSERT INTO accounts (username, password) VALUES(?,?);"

lookup_account_query :: Connection -> String -> IO [SqlValue]
lookup_account_query conn uid = do
  results <- quickQuery' conn "SELECT id, username, password FROM accounts WHERE username = ?;" [toSql uid]
  return $ if length results > 0 then head results else []

debug = debugM "db"
error = errorM "db"
