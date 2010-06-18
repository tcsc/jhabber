module Connection( Connection,
                   startConnection) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Network.Socket
import qualified Network.Socket.ByteString as Bs
import System.Log.Logger
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import XmlParse
import XMPP

data Connection = MkConn { connSocket :: Socket,
                           connBuffer :: IORef String 
                         }
  
startConnection :: Socket -> IO Connection
startConnection s = do
  buffer <- newIORef ""
  let c = MkConn s buffer
  forkIO $ runConnection c
  return c

runConnection :: Connection -> IO ()
runConnection c = do
  start <- readConnection c xmppStreamStart 
  case start of
    Just xml -> debug $ "xml: " ++ (show xml)
    Nothing -> debug "parse fail"
  connLoop c
  
connLoop :: Connection -> IO ()
connLoop c = do
  rval <- readConnection c xmppStanza 
  case rval of
    Nothing -> do 
      debug "Failure - abanoning connection"
      return ()
      
    Just xml -> do 
      connLoop c

readConnection :: Connection -> Parser a -> IO (Maybe a)
readConnection c@(MkConn socket bufferRef) parser = do
  buffer <- readIORef bufferRef
  input <- (liftM toString) $ Bs.recv socket 1024
  
  case length input of
    0 -> do -- closed connection
      debug "Connection looks like its closed"
      return Nothing
      
    _ -> do
      case parse (getRemainder . trimmed $ parser) "" (buffer ++ input) of
        Right (result, remainder) -> do
          writeIORef bufferRef remainder
          return (Just result)
          
        Left e -> do
          case head (errorMessages e) of
            SysUnExpect [] -> do
              debug "Unexpected end-of-input"
              writeIORef bufferRef (buffer ++ input)
              readConnection c parser
          
            _ -> do -- faility fail-fail
              debug $ "Parse failure: " ++ (show e)
              return Nothing

debug = debugM "conn"