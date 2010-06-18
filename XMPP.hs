module XMPP (xmppStreamStart,
             xmppStanza) where
  
import XmlParse
import Text.ParserCombinators.Parsec

xmppStreamStart :: Parser XmlElement
xmppStreamStart = do
  many $ try (trimmed xmlProcessingInstruction)
  streamTag <- xmlSimpleTag
  return streamTag
  
xmppStanza :: Parser XmlElement
xmppStanza = xmlNestedTag