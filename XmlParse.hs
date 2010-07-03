{- An XML parser based on the XMPP client library's parser that can parse XML chunks 
   without requiring the whole document
-}

module XmlParse ( XmlElement(XmlElement,XmlText),
                  XmlAttribute(XmlAttribute),
                  xmlFormatElement,
                  xmlFormatElements,
                  xmlFormatShortElement,
                  xmlNewElement,
                  xmlGetAttribute,
                  xmlGetChild,
                  xmlProcessingInstruction,
                  xmlNestedTag,
                  xmlNestedTags,
                  xmlSimpleTag,
                  xmlTrim,
                  xmlGetRemainder) where
  
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Error
import Data.List

data XmlElement = XmlElement { elemNamespace :: !String,
                               elemName :: !String,
                               attributes :: ![XmlAttribute],
                               children :: ![XmlElement] }
                | XmlText String
                deriving (Show, Eq)
                
data XmlAttribute = XmlAttribute { attrNamespace :: String, 
                                   attrName :: String, 
                                   attrValue :: String }
  deriving (Show, Eq)

xmlNewElement :: String -> XmlElement
xmlNewElement n = XmlElement "" n [] []

xmlGetAttribute :: String -> String -> XmlElement -> Maybe String
xmlGetAttribute nameSpace name (XmlElement _ _ attributes _) = do
  attrib <- find (\(XmlAttribute ns n _) ->ns == nameSpace && n == name) attributes
  return $ attrValue attrib

xmlGetChild :: XmlElement -> Int -> Maybe XmlElement
xmlGetChild e@(XmlElement _ _ _ children) n = 
  if (length children) > n then
    Just $ children !! n
  else 
    Nothing

xmlFormatElements :: Bool -> [XmlElement] -> String
xmlFormatElements short elems = (concat $ map (xmlFormatElement short) elems)

xmlFormatElement :: Bool -> XmlElement -> String
xmlFormatElement short e@(XmlElement _ _ attribs subs) =
  let fullName = xmlQualifiedName e in 
    "<" ++ fullName ++ 
      xmlFormatAttributes attribs ++ ">" ++
      if short then 
        "" 
      else
        (xmlFormatElements False subs) ++ "</" ++ fullName ++ ">"
xmlFormatElement _ (XmlText s) = s
        
xmlFormatShortElement :: XmlElement -> String
xmlFormatShortElement = xmlFormatElement True

xmlQualifiedName :: XmlElement -> String
xmlQualifiedName (XmlElement ns n _ _) =
  if ns == "" then n
  else ns ++ ":" ++ n
    
xmlFormatAttributes :: [XmlAttribute] -> String
xmlFormatAttributes [] = ""
xmlFormatAttributes attrs = concat $ 
  map (\(XmlAttribute ns name value) -> 
        let fullname = if ns == "" then name else ns ++ ":" ++ name
        in " " ++ fullname ++ "=\"" ++ value ++ "\"") attrs

xmlNestedTags :: Parser [XmlElement]
xmlNestedTags = many $ xmlNestedTag

xmlNestedTag :: Parser XmlElement
xmlNestedTag = do
  (XmlElement namespace name attributes _) <- xmlTagStart
  
  let fullname = case namespace of "" -> name; _ -> namespace ++ ":" ++ name
    
  many space
  subElements <- (try $ do {char '/'; char '>'; return []}) <|> do 
    char '>'
    elements <- many $ (try xmlNestedTag) <|> xmlText
    char '<'
    char '/'
    string fullname
    char '>'
    return elements
  return $ XmlElement namespace name attributes subElements
  
xmlSimpleTag :: Parser XmlElement
xmlSimpleTag = do 
  tag <- xmlTagStart
  char '>'
  return tag

xmlText :: Parser XmlElement
xmlText = do
    text <- many1 $ xmlPlainData <|> xmlEntity
    return $ XmlText text
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
  (namespace, name) <- xmlName
  many space
  attributes <- many $ do
    attr <- xmlAttribute
    many space
    return attr
  return $ XmlElement namespace name attributes []
  
xmlAttribute :: Parser XmlAttribute
xmlAttribute = do
  (namespace, name) <- xmlName
  char '='
  quote <- char '\'' <|> char '"'
  value <- many1 $ satisfy (/= quote)
  char quote
  return $ XmlAttribute namespace name value
  
xmlName :: Parser (String, String)
xmlName = do 
  namespace <- (try xmlNamespace) <|> string ""
  name <- many1 xmlTokenChar
  return (namespace, name)
  
xmlNamespace :: Parser String
xmlNamespace = do
  x <- many xmlTokenChar
  char ':'
  return x
  
xmlTokenChar :: Parser Char
xmlTokenChar = letter <|> char '-'

xmlTrim :: Parser a -> Parser a
xmlTrim p = do 
  spaces
  x <- p
  spaces
  return x

xmlGetRemainder :: Parser a -> Parser (a, String)
xmlGetRemainder p = do
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