module Xmpp ( Namespace(..),
              Stanza(..),
              IqAction(..),
              IqTarget(..),
              JID(..),
              Failure(..),
              RosterEntry(..),
              RosterGroup(..),
              MessageType(..),
              toXml,
              format,
              fromXml,
              newAuthFailure,
              getBindResourceName,
              parseNamespace,
              parseJid,
              formatRoster,
              formatError) where

import Codec.Binary.Base64(encode, decode)
import Control.Monad.Error
import Data.ByteString(ByteString, pack, unpack)
import Data.ByteString.UTF8(fromString, toString)
import Data.List
import Data.Typeable
import Text.Read

import Xml
import qualified XmlParse as XmlParse
import Sasl
import TextUtils

-- | Represents a Jabber ID triplet of node, host and resource ID
data JID = JID { jidName :: !String,
                 jidHost :: !String,
                 jidResource :: !String }
           deriving (Eq, Typeable)

-- | Represents a single XMPP roster entry
data RosterEntry = RosterEntry !String !JID !String ![RosterGroup]
                   deriving (Eq, Show, Typeable)

-- | Represents a group for roster contacts
data RosterGroup = RosterGroup !String
                   deriving (Eq, Show, Typeable)

instance Show JID where
  show (JID n h r) = n ++ "@" ++ h ++ (resource r)
                     where resource s = case s of
                                          [] -> []
                                          _ -> "/" ++ s

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

data MessageType = MsgTypeUnknown
                 | MsgTypeChat
                 | MsgTypeError
                 | MsgTypeGroupChat
                 | MsgTypeHeadline
                 | MsgTypeNormal
                 deriving (Show, Eq)

-- | A piece of text (optionally) tagged with a language identifier. Used to
--   hold text for the message subject and body tags
data TaggedText = Text (Maybe String) !String
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
            | Message { msgTo :: Maybe JID,
                        msgFrom :: Maybe JID,
                        msgType :: MessageType,
                        msgLang :: Maybe String,
                        msgSubject :: [TaggedText],
                        msgThread :: Maybe String,
                        msgBody :: [TaggedText],
                        msgExtra :: [XmlElement] }
            deriving (Show, Eq)

data Namespace = NsJabberClient
               | NsSasl
               | NsStreams
               | NsBind
               | NsStanzas
               | NsSession
               | NsDiscovery
               | NsRoster
               | NsXhtml
               | NsOther !String

namespaces :: [XmlAttribute]
namespaces = [ (XmlAttribute "" "xmlns" (show NsJabberClient)),
               (XmlAttribute "xmlns" "stream" (show NsStreams)) ]

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
  [XmlAttribute "" "xmlns" (show NsSasl)]
  [XmlText $ (encode . unpack . fromString) c]

toXml AuthSuccess = XmlElement "" "success" [XmlAttribute "" "xmlns" (show NsSasl)] []

toXml (Iq rid to from action content) = XmlElement "" "iq" attrs content
  where
    attrs = concat [attrTo, attrFrom, [XmlAttribute "" "type" (show action),  XmlAttribute "" "id" rid]]
    attrTo = if to == "" then [] else [XmlAttribute "" "to" to]
    attrFrom = if from == "" then [] else [XmlAttribute "" "to" from]

toXml (Message to from mType lang ss tid bs extras) = XmlElement "" "message" attrs children
  where subjects = map (textToXml "subject") ss
        bodies = map (textToXml "body") bs
        thread = maybe [] (\t -> [XmlElement "" "thread" [] [XmlText t]]) tid
        children = concat $ subjects : bodies : thread : extras : []
        attrs = concat $ [XmlAttribute "" "type" (show mType)] :
                         maybe [] (\j -> [XmlAttribute "" "to" (show j)]) to :
                         maybe [] (\j -> [XmlAttribute "" "from" (show j)]) from :
                         maybe [] (\l -> [XmlAttribute "xml" "lang" l]) lang :
                         []

{- -------------------------------------------------------------------------- -}

toXml (Failure n f) = XmlElement "" "failure" [namespace] [f]
  where namespace = XmlAttribute "" "xmlns" n

{- -------------------------------------------------------------------------- -}

textToXml :: String -> TaggedText -> XmlElement
textToXml n (Text lang text) =
    let attrs = maybe [] (\l -> [XmlAttribute "xml" "lang" l]) lang
        body = XmlText text
    in  XmlElement "" n attrs [body]

{- -------------------------------------------------------------------------- -}

-- | Parses an XML stanza into a (possible) XMPP message
fromXml :: XmlElement -> Maybe Stanza
fromXml xml@(XmlElement ns n _ _) =
  let fullName = (parseNamespace ns, n) in
  fromXmlElement fullName xml

{- -------------------------------------------------------------------------- -}

fromXmlElement :: (Namespace, String) -> XmlElement -> Maybe Stanza
fromXmlElement (NsStreams, "stream") element = do
  to <- getAttribute (show NsJabberClient) "to" element
  verText <- getAttribute (show NsJabberClient) "version" element
  ver <- readM verText
  return $ RxStreamOpen to ver

fromXmlElement (NsSasl, "auth") element = do
  m <- getAttr NsSasl "mechanism" element
  return $ AuthMechanism m

fromXmlElement (NsSasl, "response") element = do
  let mchild = getChild element 0
  case mchild of
    Just (XmlText s) -> do
      bytes <- decode s
      return $ AuthResponse (toString $ pack bytes)
    Nothing -> return $ AuthResponse ""

fromXmlElement (NsJabberClient, "iq") xml = do
  let to = maybe "" id $ getXmppAttr "to" xml
  let from = maybe "" id $ getXmppAttr "from" xml
  action <- getXmppAttr "type" xml >>= parseIqType
  id <- getXmppAttr "id" xml
  content <- getChild xml 0
  return $ Iq { iqRequestId = id,
                iqTo = to,
                iqFrom = from,
                iqAction = action,
                iqContent = (children xml) }

fromXmlElement (NsJabberClient, "message") xml = do
  let to = getXmppAttr "to" xml >>= parseJid
  let from = getXmppAttr "from" xml >>= parseJid
  let mType = maybe MsgTypeUnknown (id) (getXmppAttr "type" xml >>= readMessageType)
  let subjects = msgSubjectList xml
  let bodies = msgBodyList xml
  let everythingElse = selectChildren ( \(XmlElement _ n _ _) -> not (n == "body" || n == "subject")) xml
  return $ Message { msgTo = to,
                     msgFrom = from,
                     msgType = mType,
                     msgLang = Nothing,
                     msgSubject = subjects,
                     msgThread = Nothing,
                     msgBody = bodies,
                     msgExtra = everythingElse }

fromXmlElement _ _ = Nothing

{- -------------------------------------------------------------------------- -}

msgSubjectList :: XmlElement -> [TaggedText]
msgSubjectList xml = map taggedText subjects
  where subjects = getNamedChildren (show NsJabberClient) "subject" xml

{- -------------------------------------------------------------------------- -}

msgBodyList :: XmlElement -> [TaggedText]
msgBodyList xml = map taggedText bodies
  where bodies = getNamedChildren (show NsJabberClient) "body" xml

{- -------------------------------------------------------------------------- -}

taggedText :: XmlElement -> TaggedText
taggedText xml =
  let lang = getAttribute "xml" "lang" xml
      text = maybe "" (id) $ getChildText xml
  in Text lang text

{- -------------------------------------------------------------------------- -}

getBindResourceName :: XmlElement -> String
getBindResourceName xml = maybe "" id $ do
  child <- getNamedChild (show NsBind) "resource" xml
  name <- getChildText child
  return name

{- -------------------------------------------------------------------------- -}

iqTargetFromXml :: XmlElement -> Maybe IqTarget
iqTargetFromXml xml@(XmlElement nsBind "bind" _ _) =
  Just (BindResource $ getResourceName xml)
  where
    getResourceName :: XmlElement -> Maybe String
    getResourceName x = do
      child <- getNamedChild (show NsBind) "resource" xml
      name <- getChildText child
      return name

iqTargetFromXml xml@(XmlElement nsSession "session" _ _)  = Just Session

iqTargetFromXml xml@(XmlElement nsDiscovery "query" _ _) = Just ItemQuery

iqTargetFromXml _ = Nothing

{- -------------------------------------------------------------------------- -}

formatRoster :: [RosterEntry] -> [XmlElement]
formatRoster r = map formatEntry r
  where
    formatEntry (RosterEntry _ jid name groups) =
      XmlElement "" "item" [XmlAttribute "" "jid" (show jid),
                            XmlAttribute "" "name" name,
                            XmlAttribute "" "subscription" "both"] (formatGroups groups)
    formatGroups gs = map (\(RosterGroup g) -> XmlElement "" "group" [] [XmlText g]) gs


{- -------------------------------------------------------------------------- -}

iqTargetToXml :: IqTarget -> [XmlElement]
iqTargetToXml (BoundJid jid) =
  let resource = XmlElement "" "resource" [] [XmlText (show jid)] in
  [XmlElement "" "bind" [XmlAttribute "" "xmlns" (show NsBind)] [resource]]

iqTargetToXml (ErrorCode err) =
  let errType = XmlElement "" (show err) [XmlAttribute "" "xmlns" (show NsStanzas)] []
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
  let  errXml = XmlElement "" errName [XmlAttribute "" "xmlns" (show NsStanzas)] []
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

readMessageType :: String -> Maybe MessageType
readMessageType s = case s of
  "chat" -> Just MsgTypeChat
  "error" -> Just MsgTypeError
  "roupchat" -> Just MsgTypeGroupChat
  "headline" -> Just MsgTypeHeadline
  "normal" -> Just MsgTypeNormal
  _ -> Nothing

{- -------------------------------------------------------------------------- -}

newAuthFailure :: String -> Stanza
newAuthFailure f = Failure (show NsSasl) child
 where child = newElement f

getAttr :: Namespace -> String -> XmlElement -> Maybe String
getAttr ns n xml = getAttribute (show ns) n xml

getXmppAttr = getAttr NsJabberClient

{- -------------------------------------------------------------------------- -}

parseNamespace :: String -> Namespace
parseNamespace "jabber:client" = NsJabberClient
parseNamespace "urn:ietf:params:xml:ns:xmpp-sasl" = NsSasl
parseNamespace "http://etherx.jabber.org/streams" = NsStreams
parseNamespace "urn:ietf:params:xml:ns:xmpp-bind" = NsBind
parseNamespace "urn:ietf:params:ns:xmpp-stanzas" = NsStanzas
parseNamespace "urn:ietf:params:xml:ns:xmpp-session" = NsSession
parseNamespace "http://jabber.org/protocol/disco#items" = NsDiscovery
parseNamespace "jabber:iq:roster" = NsRoster
parseNamespace "http://www.w3.org/1999/xhtml" = NsXhtml
parseNamespace s = NsOther s

instance Show Namespace where
  show NsJabberClient = "jabber:client"
  show NsSasl = "urn:ietf:params:xml:ns:xmpp-sasl"
  show NsStreams = "http://etherx.jabber.org/streams"
  show NsBind = "urn:ietf:params:xml:ns:xmpp-bind"
  show NsStanzas = "urn:ietf:params:ns:xmpp-stanzas"
  show NsSession = "urn:ietf:params:xml:ns:xmpp-session"
  show NsDiscovery = "http://jabber.org/protocol/disco#items"
  show NsRoster = "jabber:iq:roster"
  show NsXhtml = "http://www.w3.org/1999/xhtml"
  show (NsOther s) = s

{- -------------------------------------------------------------------------- -}

textFilter :: XmlElement -> Bool
textFilter (XmlText _) = True
textFilter _ = False
