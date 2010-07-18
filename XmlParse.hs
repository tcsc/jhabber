{- An XML parser based on the XMPP client library's parser that can parse XML chunks 
   without requiring the whole document
-}

module XmlParse ( Context,
                  XmlFailure(..),
                  XmlResult,
                  newContext,
                  parsePreamble,
                  parseNestedTag,
                  parseStartTag,
                  processingInstruction,
                  processingInstructions,
                  nestedTag,
                  nestedTags,
                  simpleTag,
                  trim,
                  getRemainder) where
  
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Error
import Control.Monad.Error
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Xml

type NsTable = Map String String

data Context = Ctx !NsTable
               deriving (Show)

data XmlFailure = InsufficientData
                | NoMorePreamble
                | ParseFailure
                | Fail String
                deriving (Read, Show, Eq)
                
instance Error XmlFailure where
  strMsg = Fail

type XmlResult a = Either XmlFailure a

{- ------------------------------------------------------------------------- -}

newContext :: Context
newContext = Ctx Map.empty

{- ------------------------------------------------------------------------- -}

parsePreamble :: String -> XmlResult (String, XmlElement)
parsePreamble text = do
  case Parsec.parse (getRemainder $ trim $ preamble) "" text of
    Right (xml, remainder) -> 
      case xml of
        (XmlProcessingInstruction s) -> return (remainder, xml)
        _ -> throwError NoMorePreamble
        
    Left err -> case head (errorMessages err) of
      -- simple out-of-data error. No biggie, just go around and try again
      SysUnExpect [] -> throwError InsufficientData
        
      -- full-on parse failure... we should do something here 
      _ -> throwError ParseFailure

{- ------------------------------------------------------------------------- -}

parseNestedTag :: Context -> String -> XmlResult (String, XmlElement)
parseNestedTag (Ctx nsTable) text =
  case Parsec.parse (getRemainder $ trim (nestedTag nsTable)) "" text of
    Right (xml, remainder) -> return (remainder, xml)
    Left err -> case head (errorMessages err) of
      -- simple out-of-data error. No biggie, just go around and try again
      SysUnExpect [] -> throwError InsufficientData
        
      -- full-on parse failure... we should do something here 
      _ -> throwError ParseFailure

{- ------------------------------------------------------------------------- -}

parseStartTag :: Context -> String -> XmlResult (String, XmlElement, Context)
parseStartTag ctx@(Ctx nst) text = 
  case Parsec.parse (getRemainder $ trim simpleTag) "" text of
    Right ((nst', xml), remainder) -> do
      let nst'' = Map.union nst nst'
      let xml' = canonicalizeNames nst'' xml
      return (remainder, xml', Ctx nst'')
    Left err -> case head (errorMessages err) of
      -- simple out-of-data error. No biggie, just go around and try again
      SysUnExpect [] -> throwError InsufficientData
        
      -- full-on parse failure... we should do something here 
      _ -> throwError ParseFailure

{- ------------------------------------------------------------------------- -}

{-
parse :: Context -> String -> Parser XmlElement -> XmlResult (String, XmlElement, Context)
parse (Ctx nsTable) text p = 
  case Parsec.parse (XmlParse.getRemainder p) "" text of
    Right (xml, remainder) -> do
      let nsTable' = updateNsTable nsTable xml
          xml' = canonicalizeNames nsTable' xml
          ctx' = Ctx nsTable'
      return (remainder, xml', ctx')
      
    Left err -> case head (errorMessages err) of
      -- simple out-of-data error. No biggie, just go around and try again
      SysUnExpect [] -> throwError InsufficientData
        
      -- full-on parse failure... we should do something here 
      _ -> throwError ParseFailure
-}

{- ------------------------------------------------------------------------- -}

updateNsTable :: NsTable -> [XmlAttribute] -> NsTable
updateNsTable nsTable [] = nsTable 
updateNsTable nsTable as =  foldl' extractNs nsTable as
  where extractNs :: NsTable -> XmlAttribute -> NsTable
        extractNs t (XmlAttribute ns n v) = 
          if n == "xmlns" 
            then Map.insert "" v t
            else if ns == "xmlns"
              then Map.insert n v t 
              else t
  
{- ------------------------------------------------------------------------- -}

canonicalizeNames :: NsTable -> XmlElement -> XmlElement
canonicalizeNames nsMap (XmlElement ns n attribs children) =
  XmlElement ns' n attribs' kids'
  where ns' = lookupNs nsMap ns
        kids' = map (canonicalizeNames nsMap) children 
        attribs' = map cAttrib attribs
        cAttrib (XmlAttribute ns n v) = XmlAttribute (lookupNs nsMap ns) n v

        lookupNs :: NsTable -> String -> String
        lookupNs nsm ns = case Map.lookup ns nsm of
          Just ns' -> ns'
          Nothing -> ""

canonicalizeNames _ element = element

{- ------------------------------------------------------------------------- -}

nsLookup :: NsTable -> String -> String
nsLookup nsm ns = case Map.lookup ns nsm of
  Just ns' -> ns'
  Nothing -> ""

{- ------------------------------------------------------------------------- -}

nestedTags :: NsTable -> Parser [XmlElement]
nestedTags nst = many $ nestedTag nst

nestedTag :: NsTable -> Parser XmlElement
nestedTag nst = 
  do
    (nst', (XmlElement namespace name attrs _)) <- tagStart nst
    let fullname = case namespace of "" -> name; _ -> namespace ++ ":" ++ name
    
    many space
    subElements <- (try $ do {char '/'; char '>'; return []}) <|> do 
      char '>'
      elements <- many $ (try $ nestedTag nst') <|> text
      char '<'
      char '/'
      string fullname
      char '>'
      return elements
    
    let attrs' = foldl' (filterAttribs nst') [] attrs
    let namespace' = (nsLookup nst' namespace)
  
    return $ XmlElement namespace' name attrs' subElements

filterAttribs :: NsTable -> [XmlAttribute] -> XmlAttribute -> [XmlAttribute]
filterAttribs nst acc (XmlAttribute ns n v) = 
  if ns == "xmlns" || n == "xmlns" 
    then acc
    else (XmlAttribute (nsLookup nst ns) n v) : acc
  
{- ------------------------------------------------------------------------- -}
  
simpleTag :: Parser (NsTable, XmlElement)
simpleTag = do 
  (nst, tag) <- tagStart Map.empty
  char '>'
  return (nst, tag)

{- ------------------------------------------------------------------------- -}

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

{- ------------------------------------------------------------------------- -}
  
tagStart :: NsTable -> Parser (NsTable, XmlElement)
tagStart nst = do
  char '<'
  (namespace, name) <- elementName
  many space
  (nst', attrs) <- XmlParse.attributes nst
  return (nst', XmlElement namespace name attrs [])

attributes :: NsTable -> Parser (NsTable, [XmlAttribute])
attributes nst = do
    as <- many $ do 
      (nst', attr) <- attribute nst
      many space 
      return (nst', attr)
    return $ foldl' unpack (nst, []) as
  where
    unpack :: (NsTable, [XmlAttribute]) -> 
              (Maybe (String, String), XmlAttribute) -> 
              (NsTable, [XmlAttribute])
    unpack (nst, attrs) (maybeNs, attr) = 
      let nst' = maybe nst (\(s,l) -> Map.insert s l nst) maybeNs
      in (nst', attr : attrs)
        
{- ------------------------------------------------------------------------- -}
  
attribute :: NsTable -> Parser (Maybe (String, String), XmlAttribute)
attribute nst = do
    (ns, name) <- elementName
    char '='
    quote <- char '\'' <|> char '"'
    value <- many1 $ satisfy (/= quote)
    char quote

    let (ns', nst') = scanNamespaces ns name value nst

    return (nst', XmlAttribute ns' name value)
  where
    scanNamespaces :: String -> String -> String -> NsTable -> (String, Maybe (String, String))
    scanNamespaces ns n v nst = 
      if n == "xmlns"
        then ("", Just ("",v))
          else if ns == "xmlns" 
            then (ns, Just(n,v))
            else ((nsLookup nst ns), Nothing)
    
elementName :: Parser (String, String)
elementName = do 
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

preamble :: Parser XmlElement
preamble = (try processingInstruction) <|> do
  (_,xml) <- simpleTag
  return xml
  
processingInstructions :: Parser ([XmlElement])
processingInstructions = many processingInstruction
  
processingInstruction :: Parser XmlElement
processingInstruction = do
  char '<'
  char '?'
  text <- many $ satisfy (/= '?')
  char '?'
  char '>'
  return $ XmlProcessingInstruction text