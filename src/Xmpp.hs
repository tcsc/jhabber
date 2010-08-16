module Xmpp ( Stanza(..),
              IqAction(..),
              IqTarget(..),
              JID(..),
              toXml,
              format,
              fromXml,
              newAuthFailure) where

import Codec.Binary.Base64(encode, decode)
import Data.ByteString(ByteString, pack, unpack)
import Data.ByteString.UTF8(fromString, toString)
import Data.List

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

data IqTarget = None
              | BindResource (Maybe String)
              | BoundJid JID
              | Session
              deriving (Show, Eq)

data Stanza = RxStreamOpen String Float
            | TxStreamOpen String Integer
            | Features [XmlElement]
            | AuthMechanism String
            | AuthChallenge String
            | AuthResponse String
            | AuthSuccess
            | Failure String XmlElement
            | Iq String IqAction IqTarget
            deriving (Show, Eq)

nsJabber = "jabber:client"
nsStreams = "http://etherx.jabber.org/streams"
nsBind = "urn:ietf:params:xml:ns:xmpp-bind"
nsSession = "urn:ietf:params:xml:ns:xmpp-session"

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

toXml (Iq rid action t) =
  let attrs = [XmlAttribute "" "type" (show action), XmlAttribute "" "id" rid]
      target = iqTargetToXml t
  in  XmlElement "" "iq" attrs [target]

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

fromXml element@(XmlElement nsJabber "iq" _ _) = do
  action <- getAttribute nsJabber "type" element >>= parseIqType
  id <- getAttribute nsJabber "id" element
  target <- getChild element 0 >>= iqTargetFromXml
  return $ Iq id action target

fromXml element = Nothing

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

iqTargetFromXml _ = Nothing

{- -------------------------------------------------------------------------- -}

iqTargetToXml :: IqTarget -> XmlElement
iqTargetToXml (BoundJid jid) =
  let resource = XmlElement "" "resource" [] [XmlText (show jid)] in
  XmlElement "" "bind" [XmlAttribute "" "xmlns" nsBind] [resource]

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
