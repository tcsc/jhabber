{- An XML parser based on the XMPP client library's parser that can parse XML chunks 
   without requiring the whole document
-}

module XmlParse where
  
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.List

data XmlElement = Element String [(String,String)] [XmlElement]
                | Text String
                deriving (Show, Eq)

-- | Get an attribute value
getAttribute :: String -> XmlElement -> Maybe String
getAttribute name (Element _ attributes _) = lookup name attributes 

xmlNestedTags :: Parser [XmlElement]
xmlNestedTags = many $ xmlNestedTag

xmlNestedTag :: Parser XmlElement
xmlNestedTag = do
  (Element name attributes _) <- xmlTagStart
  many space
  subElements <- (try $ do {char '/'; char '>'; return []}) <|> do 
    char '>'
    elements <- many $ (try xmlNestedTag) <|> xmlText
    char '<'
    char '/'
    string name
    char '>'
    return elements
  return $ Element name attributes subElements

xmlSimpleTag :: Parser XmlElement
xmlSimpleTag = do 
  tag <- xmlTagStart
  char '>'
  return tag

xmlText :: Parser XmlElement
xmlText = do
    text <- many1 $ xmlPlainData <|> xmlEntity
    return $ Text text
  where xmlPlainData = satisfy (\c -> (c /= '<') && (c /= '&') )
        xmlEntity = do
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
  
xmlTagStart :: Parser XmlElement
xmlTagStart = do
  char '<'
  name <- many1 xmlTokenChar
  many space
  attributes <- many $ do
    attr <- xmlAttribute
    many space
    return attr
  return $ Element name attributes []
  
xmlAttribute :: Parser (String, String)
xmlAttribute = do
  name <- many1 xmlTokenChar
  char '='
  quote <- char '\'' <|> char '"'
  value <- many1 $ satisfy (/= quote)
  char quote
  return (name, value)
  
xmlTokenChar :: Parser Char
xmlTokenChar = letter <|> char ':' <|> char '-'

trimmed :: Parser a -> Parser a
trimmed p = do 
  spaces
  x <- p
  spaces
  return x

getRemainder :: Parser a -> Parser (a, String)
getRemainder p = do
  x <- try p
  r <- getInput
  return (x,r)

xmlProcessingInstruction :: Parser ()
xmlProcessingInstruction = do
  char '<'
  char '?'
  many $ satisfy (/= '?')
  char '?'
  char '>'
  return ()