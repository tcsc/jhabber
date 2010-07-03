module XMPP ( XmppStanza(..),
              xmppParseStreamStart,
              xmppParseStanza,
              xmppFormat,
              xmppFromXml,
              xmppNewAuthFailure) where
                
import Codec.Binary.Base64(encode, decode)
import Data.ByteString(ByteString, pack, unpack)
import Data.ByteString.UTF8(fromString, toString)
import Data.Digest.MD5(hash)
import Text.ParserCombinators.Parsec

import XmlParse
import Sasl

data XmppStanza = RxStreamOpen String Integer
                | Features [XmlElement]
                | AuthMechanism String
                | AuthChallenge String
                | AuthResponse String
                | Failure String XmlElement
                deriving (Show)

xmppNamespaces :: [XmlAttribute]
xmppNamespaces = [
  (XmlAttribute "" "xmlns" "jabber:client"),
  (XmlAttribute "xmlns" "stream" "http://etherx.jabber.org/streams") ]

xmppParseStreamStart :: Parser XmlElement
xmppParseStreamStart = do
  many $ try (xmlTrim xmlProcessingInstruction)
  streamTag <- xmlSimpleTag
  return streamTag
  
xmppRxStreamStart :: String -> Integer -> XmlElement
xmppRxStreamStart rx sId = 
    XmlElement "stream" "stream" attribs []
  where 
    attribs = concat [xmppNamespaces, ([
      (XmlAttribute "" "from" rx),
      (XmlAttribute "" "id" $ show sId),
      (XmlAttribute "" "version" "1.0") ])]
  
xmppParseStanza :: Parser XmlElement
xmppParseStanza = xmlNestedTag

{-| Takes an XMPP message and formats it as a chunk of ready-to-send UTF-8 encoded XML -}  
xmppFormat :: XmppStanza -> ByteString
xmppFormat (RxStreamOpen host connId) = 
  fromString $ xmlFormatShortElement $ xmppRxStreamStart host connId
  
xmppFormat stanza = fromString $ xmlFormatElement False $ xmppToXml stanza

{-| Takes an XMPP message and formats it as an XML element -}
xmppToXml :: XmppStanza -> XmlElement
xmppToXml (Features fs) = XmlElement "stream" "features" [] fs

xmppToXml (AuthChallenge c) = XmlElement "" "challenge"
  [XmlAttribute "" "xmlns" saslNamespace] 
  [XmlText $ (encode . unpack . fromString) c]

xmppToXml (Failure n f) = XmlElement "" "failure" [namespace] [f]
  where namespace = XmlAttribute "" "xmlns" n

{-| Parses an XML stanza into a (possible) XMPP message -}
xmppFromXml :: XmlElement -> Maybe XmppStanza
xmppFromXml element@(XmlElement "" "auth" attribs children) = do 
  ns <- xmlGetAttribute "" "xmlns" element
  m <- xmlGetAttribute "" "mechanism" element
  return $ AuthMechanism m
  
xmppFromXml element@(XmlElement "" "response" attribs children) = do 
  ns <- xmlGetAttribute "" "xmlns" element
  child <- xmlGetChild element 0
  case child of
    XmlText s -> do 
      bytes <- decode s
      return $ AuthResponse (toString $ pack bytes)
    _ -> Nothing
      
xmppFromXml element = Nothing
  
xmppNewAuthFailure :: String -> XmppStanza
xmppNewAuthFailure f = Failure (saslNamespace) child
 where child = xmlNewElement f
  
textFilter :: XmlElement -> Bool
textFilter (XmlText _) = True
textFilter _ = False