module Xmpp ( Stanza(..),
              IqAction(..),
              IqTarget(..),
              JID(..),
              Failure(..),
              toXml,
              format,
              fromXml,
              newAuthFailure,
              getBindResourceName,
              formatError) where

import Codec.Binary.Base64(encode, decode)
import Control.Monad.Error
import Data.ByteString(ByteString, pack, unpack)
import Data.ByteString.UTF8(fromString, toString)
import Data.List
import Text.Read

import Xml
import qualified XmlParse as XmlParse
import Sasl
import TextUtils

-- | Represents a Jabber ID triplet of node, host and resource ID
data JID = JID { jidName :: !String,
                 jidHost :: !String,
                 jidResource :: !String }
           deriving (Eq)

instance Show JID where
  show (JID n h r) = n ++ "@" ++ h ++ "/" ++ r

data IqAction = Set | Get | Result | Error
  deriving (Eq)

instance Show IqAction where
  show n = case n of
            Set -> "set"
            Get -> "get"
            Result -> "result"
            Error -> "error"

data NamedItem = NamedItem JID String
               deriving (Show, Eq)

data Failure = NotAllowed | ServiceUnavailable
             deriving (Eq, Read)

instance Show Failure where
  show f = case f of
            NotAllowed -> "not-allowed"
            ServiceUnavailable -> "service-unavailable"

instance Error Failure where
  strMsg x = read x

data IqTarget = None
              | BindResource (Maybe String)
              | BoundJid JID
              | ItemQuery
              | QueryResponse [NamedItem]
              | Session
              | ErrorCode Failure
              deriving (Show, Eq)

data Stanza = RxStreamOpen String Float
            | TxStreamOpen String Integer
            | Features [XmlElement]
            | AuthMechanism String
            | AuthChallenge String
            | AuthResponse String
            | AuthSuccess
            | Failure String XmlElement
            | Iq { iqRequestId :: !String,
                   iqTo :: !String,
                   iqFrom :: !String,
                   iqAction :: !IqAction,
                   iqContent :: [XmlElement] }
            deriving (Show, Eq)

nsJabber = "jabber:client"
nsStreams = "http://etherx.jabber.org/streams"
nsBind = "urn:ietf:params:xml:ns:xmpp-bind"
nsStanzas = "urn:ietf:params:ns:xmpp-stanzas"
nsSession = "urn:ietf:params:xml:ns:xmpp-session"
nsDiscovery = "http://jabber.org/protocol/disco#items"

namespaces :: [XmlAttribute]
namespaces = [
  (XmlAttribute "" "xmlns" nsJabber),
  (XmlAttribute "xmlns" "stream" nsStreams) ]

txStreamStart :: String -> Integer -> XmlElement
txStreamStart rx sId =
    XmlElement "stream" "stream" attribs []
  where
    attribs = concat [namespaces, ([
      (XmlAttribute "" "from" rx),
      (XmlAttribute "" "id" $ show sId),
      (XmlAttribute "" "version" "1.0") ])]

-- | Takes an XMPP message and formats it as a chunk of ready-to-send UTF-8
--   encoded XML
format :: Stanza -> ByteString
format (TxStreamOpen host connId) = fromString . formatShortElement $ txStreamStart host connId
format stanza = fromString . formatElement False $ toXml stanza

{- -------------------------------------------------------------------------- -}

{-| Takes an XMPP message and formats it as an XML element -}
toXml :: Stanza -> XmlElement
toXml (Features fs) = XmlElement "stream" "features" [] fs

toXml (AuthChallenge c) = XmlElement "" "challenge"
  [XmlAttribute "" "xmlns" nsSasl]
  [XmlText $ (encode . unpack . fromString) c]

toXml AuthSuccess = XmlElement "" "success" [XmlAttribute "" "xmlns" nsSasl] []

toXml (Iq rid to from action content) = XmlElement "" "iq" attrs content
  where
    attrs = concat [attrTo, attrFrom, [XmlAttribute "" "type" (show action),  XmlAttribute "" "id" rid]]
    attrTo = if to == "" then [] else [XmlAttribute "" "to" to]
    attrFrom = if from == "" then [] else [XmlAttribute "" "to" from]

toXml (Failure n f) = XmlElement "" "failure" [namespace] [f]
  where namespace = XmlAttribute "" "xmlns" n

{- -------------------------------------------------------------------------- -}

-- | Parses an XML stanza into a (possible) XMPP message
fromXml :: XmlElement -> Maybe Stanza
fromXml element@(XmlElement nsStreams "stream" attribs _) = do
  to <- getAttribute nsJabber "to" element
  verText <- getAttribute nsJabber "version" element
  ver <- readM verText
  return $ RxStreamOpen to ver

fromXml element@(XmlElement nsSasl "auth" attribs children) = do
  m <- getAttribute nsSasl "mechanism" element
  return $ AuthMechanism m

fromXml element@(XmlElement nsSasl "response" attribs children) = do
  let mchild = getChild element 0
  case mchild of
    Just (XmlText s) -> do
      bytes <- decode s
      return $ AuthResponse (toString $ pack bytes)
    Nothing -> return $ AuthResponse ""

fromXml xml@(XmlElement nsJabber "iq" _ children) = do
  let to = maybe "" id $ getAttribute nsJabber "to" xml
  let from = maybe "" id $ getAttribute nsJabber "from" xml
  action <- getAttribute nsJabber "type" xml >>= parseIqType
  id <- getAttribute nsJabber "id" xml
  content <- getChild xml 0
  return $ Iq { iqRequestId = id,
                iqTo = to,
                iqFrom = from,
                iqAction = action,
                iqContent = children }

fromXml element = Nothing

{- -------------------------------------------------------------------------- -}

getBindResourceName :: XmlElement -> String
getBindResourceName xml = maybe "" id $ do
  child <- getNamedChild nsBind "resource" xml
  name <- getChildText child
  return name

{- -------------------------------------------------------------------------- -}

iqTargetFromXml :: XmlElement -> Maybe IqTarget
iqTargetFromXml xml@(XmlElement nsBind "bind" _ _) =
  Just (BindResource $ getResourceName xml)
  where
    getResourceName :: XmlElement -> Maybe String
    getResourceName x = do
      child <- getNamedChild nsBind "resource" xml
      name <- getChildText child
      return name

iqTargetFromXml xml@(XmlElement nsSession "session" _ _)  = Just Session

iqTargetFromXml xml@(XmlElement nsDiscovery "query" _ _) = Just ItemQuery

iqTargetFromXml _ = Nothing

{- -------------------------------------------------------------------------- -}

iqTargetToXml :: IqTarget -> [XmlElement]
iqTargetToXml (BoundJid jid) =
  let resource = XmlElement "" "resource" [] [XmlText (show jid)] in
  [XmlElement "" "bind" [XmlAttribute "" "xmlns" nsBind] [resource]]

iqTargetToXml (ErrorCode err) =
  let errType = XmlElement "" (show err) [XmlAttribute "" "xmlns" nsStanzas] []
  in
  [XmlElement "" "error" [XmlAttribute "" "type" "cancel"] [errType]]

iqTargetToXml Xmpp.None = []

{- -------------------------------------------------------------------------- -}

parseJid :: String -> Maybe JID
parseJid s =
  case elemIndex '@' s of
    Nothing -> Nothing
    Just n1 -> let node = take n1 s
                   s' = drop (n1+1) s
                   n2 = maybe (length s') id (elemIndex '/' s')
               in Just $ JID node (take n2 s') (drop (n2 + 1) s')

{- -------------------------------------------------------------------------- -}

formatError :: String -> String -> XmlElement
formatError errType errName =
  let  errXml = XmlElement "" errName [XmlAttribute "" "xmlns" nsStanzas] []
  in XmlElement "" "error" [XmlAttribute "" "type" errType] [errXml]

{- -------------------------------------------------------------------------- -}

parseIqType :: String -> Maybe IqAction
parseIqType s = case s of
  "set" -> return Set
  "get" -> return Get
  "result" -> return Result
  "error" -> return Error
  _ -> Nothing

{- -------------------------------------------------------------------------- -}

newAuthFailure :: String -> Stanza
newAuthFailure f = Failure (nsSasl) child
 where child = newElement f

{- -------------------------------------------------------------------------- -}

textFilter :: XmlElement -> Bool
textFilter (XmlText _) = True
textFilter _ = False
