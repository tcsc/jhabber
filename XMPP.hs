module XMPP ( Stanza(..),
              toXml,
              format,
              fromXml,
              newAuthFailure) where
                
import Codec.Binary.Base64(encode, decode)
import Data.ByteString(ByteString, pack, unpack)
import Data.ByteString.UTF8(fromString, toString)
import Data.Digest.MD5(hash)

import Xml
import qualified XmlParse as XmlParse
import Sasl
import TextUtils

data IqAction = Set
              | Get
              | Result
              | Error
              deriving (Show)

data Stanza = RxStreamOpen String Float
            | TxStreamOpen String Integer
            | Features [XmlElement]
            | AuthMechanism String
            | AuthChallenge String
            | AuthResponse String
            | AuthSuccess
            | Failure String XmlElement
            | Iq IqAction String
            deriving (Show)

nsJabber = "jabber:client"
nsStreams = "http://etherx.jabber.org/streams"

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
        
{-| Takes an XMPP message and formats it as a chunk of ready-to-send UTF-8 encoded XML -}  
format :: Stanza -> ByteString
format (TxStreamOpen host connId) = fromString . formatShortElement $ txStreamStart host connId
format stanza = fromString . formatElement False $ toXml stanza

{-| Takes an XMPP message and formats it as an XML element -}
toXml :: Stanza -> XmlElement
toXml (Features fs) = XmlElement "stream" "features" [] fs

toXml (AuthChallenge c) = XmlElement "" "challenge"
  [XmlAttribute "" "xmlns" saslNamespace] 
  [XmlText $ (encode . unpack . fromString) c]

toXml AuthSuccess = XmlElement "" "success" [XmlAttribute "" "xmlns" saslNamespace] []

toXml (Failure n f) = XmlElement "" "failure" [namespace] [f]
  where namespace = XmlAttribute "" "xmlns" n

{-| Parses an XML stanza into a (possible) XMPP message -}
fromXml :: XmlElement -> Maybe Stanza
fromXml element@(XmlElement nsStreams "stream" attribs _) = do
  to <- getAttribute nsJabber "to" element
  verText <- getAttribute nsJabber "version" element
  ver <- readM verText
  return $ RxStreamOpen to ver

fromXml element@(XmlElement nsJabber "auth" attribs children) = do 
  ns <- getAttribute nsJabber "xmlns" element
  m <- getAttribute nsJabber "mechanism" element
  return $ AuthMechanism m
  
fromXml element@(XmlElement nsJabber "response" attribs children) = do 
  ns <- getAttribute nsJabber "xmlns" element
  let mchild = getChild element 0
  case mchild of
    Just (XmlText s) -> do
      bytes <- decode s
      return $ AuthResponse (toString $ pack bytes)
    Nothing -> return $ AuthResponse ""
    
--fromXml element@(XmlElement "" "iq")
      
fromXml element = Nothing
  
newAuthFailure :: String -> Stanza
newAuthFailure f = Failure (saslNamespace) child
 where child = newElement f
  
textFilter :: XmlElement -> Bool
textFilter (XmlText _) = True
textFilter _ = False