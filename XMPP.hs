module XMPP ( XmppStanza( RxStreamOpen, Features, AuthMechanism, AuthChallenge),
              xmppParseStreamStart,
              xmppParseStanza,
              xmppFormat,
              xmppFromXml) where
                
import Codec.Binary.Base64(encode, decode)
import Data.ByteString(ByteString, unpack)
import Data.ByteString.UTF8(fromString)
import Data.Digest.MD5(hash)
import Text.ParserCombinators.Parsec
import XmlParse

data XmppStanza = RxStreamOpen String Integer
                | Features [XmlElement]
                | AuthMechanism String
                | AuthChallenge String
                deriving (Show)
                
saslNamespace :: String
saslNamespace = "urn:ietf:params:xml:ns:xmpp-sasl"

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

xmppXml :: XmppStanza -> XmlElement
xmppXml (Features fs) = XmlElement "stream" "features" [] fs
xmppXml (AuthChallenge c) = XmlElement "" "challenge"
  [XmlAttribute "" "xmlns" saslNamespace] 
  [XmlText $ (encode . unpack . fromString) c]
  
xmppFormat :: XmppStanza -> ByteString
xmppFormat (RxStreamOpen host connId) = 
  fromString $ xmlFormatShortElement $ xmppRxStreamStart host connId
xmppFormat stanza = 
  fromString $ xmlFormatElement False $ xmppXml stanza
  
xmppFromXml :: XmlElement -> Maybe XmppStanza
xmppFromXml element@(XmlElement "" "auth" attribs children) = do 
  ns <- xmlGetAttribute "" "xmlns" element
  m <- xmlGetAttribute "" "mechanism" element
  return $ AuthMechanism m
xmppFromXml element = Nothing

  
textFilter :: XmlElement -> Bool
textFilter (XmlText _) = True
textFilter _ = False