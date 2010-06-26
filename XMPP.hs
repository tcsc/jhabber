module XMPP ( XmppStanza( RxStreamOpen, Features ),
              xmppParseStreamStart,
              xmppParseStanza,
              xmppFormat) where
  
import Data.ByteString.UTF8
import Text.ParserCombinators.Parsec
import XmlParse

data XmppStanza = RxStreamOpen String Integer
                | Features [XmlElement]
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

xmppXml :: XmppStanza -> XmlElement
xmppXml (Features fs) = XmlElement "stream" "features" [] fs
  
xmppFormat :: XmppStanza -> ByteString
xmppFormat (RxStreamOpen host connId) = 
  fromString $ xmlFormatShortElement $ xmppRxStreamStart host connId
xmppFormat stanza = 
  fromString $ xmlFormatElement False $ xmppXml stanza