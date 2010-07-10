{- An XML parser based on the XMPP client library's parser that can parse XML chunks 
   without requiring the whole document
-}

module XmlParse ( processingInstruction,
                  processingInstructions,
                  nestedTag,
                  nestedTags,
                  simpleTag,
                  trim,
                  getRemainder) where
  
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Error
import Data.List
import Xml

nestedTags :: Parser [XmlElement]
nestedTags = many $ nestedTag

nestedTag :: Parser XmlElement
nestedTag = do
  (XmlElement namespace name attributes _) <- tagStart
  
  let fullname = case namespace of "" -> name; _ -> namespace ++ ":" ++ name
    
  many space
  subElements <- (try $ do {char '/'; char '>'; return []}) <|> do 
    char '>'
    elements <- many $ (try nestedTag) <|> text
    char '<'
    char '/'
    string fullname
    char '>'
    return elements
  return $ XmlElement namespace name attributes subElements
  
simpleTag :: Parser XmlElement
simpleTag = do 
  tag <- tagStart
  char '>'
  return tag

text :: Parser XmlElement
text = do
    text <- many1 $ plainData <|> escapedChar
    return $ XmlText text
  where plainData = satisfy (\c -> (c /= '<') && (c /= '&') )
        escapedChar = do
          char '&'
          entity <- try (string "amp") <|>
                    try (string "lt") <|>
                    try (string "gt") <|>
                    try (string "quot") <|>
                    string "apos"
          char ';'
          return $ case entity of
            "amp" -> '&'
            "lt" -> '<'
            "gt" -> '>'
            "quot" -> '"'
            "apos" -> '\''
  
tagStart :: Parser XmlElement
tagStart = do
  char '<'
  (namespace, name) <- name
  many space
  attributes <- many $ do
    attr <- attribute
    many space
    return attr
  return $ XmlElement namespace name attributes []
  
attribute :: Parser XmlAttribute
attribute = do
  (namespace, name) <- name
  char '='
  quote <- char '\'' <|> char '"'
  value <- many1 $ satisfy (/= quote)
  char quote
  return $ XmlAttribute namespace name value
  
name :: Parser (String, String)
name = do 
  namespace <- (try namespace) <|> string ""
  name <- many1 tokenChar
  return (namespace, name)
  
namespace :: Parser String
namespace = do
  x <- many tokenChar
  char ':'
  return x
  
tokenChar :: Parser Char
tokenChar = letter <|> char '-'

trim :: Parser a -> Parser a
trim p = do 
  spaces
  x <- p
  spaces
  return x

getRemainder :: Parser a -> Parser (a, String)
getRemainder p = do
  x <- try p
  r <- getInput
  return (x,r)

processingInstructions :: Parser [XmlElement]
processingInstructions = many processingInstruction
  
processingInstruction :: Parser XmlElement
processingInstruction = do
  char '<'
  char '?'
  text <- many $ satisfy (/= '?')
  char '?'
  char '>'
  return $ XmlProcessingInstruction text