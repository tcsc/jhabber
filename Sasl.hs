module Sasl ( AuthInfo(None), 
              newAuthInfo, 
              challenge,
              checkResponse,
              checkCredentials,
              getUid,
              saslNamespace ) where
  
import Data.Char
import Data.Hex
import Data.List
import System.Random
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Error
import System.Log.Logger(debugM)
import System.IO.Unsafe

data AuthInfo = None
              | Basic String 
              | Digest { digRealm :: !String,
                         digNonce :: !String,
                         digUid :: !String,
                         digCNonce :: !String,
                         digResponse :: !String }
              deriving (Show)

saslNamespace :: String
saslNamespace = "urn:ietf:params:xml:ns:xmpp-sasl"
              
newAuthInfo :: String -> String -> IO (Maybe AuthInfo)
newAuthInfo "DIGEST-MD5" realm = do
  g <- newStdGen
  let n = hex . map chr . take 16 $ (randomRs (0, 255) g :: [Int])
  return $ Just (newDigest realm n)
newAuthInfo _ _ = return Nothing

newDigest :: String -> String -> AuthInfo
newDigest realm nonce = Digest realm nonce "" "" ""
  
challenge :: AuthInfo -> String
challenge (Digest realm nonce _ _ _) = 
  "realm=\"" ++ realm ++ "\"," ++ 
  "nonce=\"" ++ nonce ++ "\"," ++ 
  "qop=\"auth\"," ++ 
  "charset=\"utf-8\"," ++
  "algorithm=\"md5-sess\""
                                 
--parseChallenge :: AuthInfo -> String -> Maybe [String,String]
--parseChallenge (Digest _ _) response = do
  
checkResponse :: AuthInfo -> String -> Maybe AuthInfo
checkResponse auth@(Digest _ _ _ _ _) r = do
  pairs <- parsePairs r
  response <- lookup "response" pairs
  nonce <- lookup "nonce" pairs
  realm <- lookup "realm" pairs
  clientNonce <- lookup "cnonce" pairs
  uid <- lookup "username" pairs
  if (nonce /= (digNonce auth)) || (realm /= (digRealm auth)) 
    then Nothing
    else return auth { digUid = uid, digCNonce = clientNonce, digResponse = response }

checkCredentials :: AuthInfo -> String -> String -> Bool
checkCredentials auth@(Digest _ _ _ _ _) uid pwd = False
    
checkCredentials a uid pwd = False

getUid :: AuthInfo -> String
getUid auth@(Digest _ _ _ _ _) = digUid auth

parsePairs :: String -> Maybe [(String, String)]
parsePairs s = case parse pairs "" s of
                 Right r -> Just r
                 _ -> Nothing
  
pairs :: Parser [(String,String)]
pairs = do
  many pair
  
pair :: Parser (String,String)
pair = do
  name <- many1 validNameChar
  char '='
  value <- (try quotedValue) <|> unquotedValue
  optional $ char ','
  return (name, value)
  
unquotedValue :: Parser String
unquotedValue = many1 $ satisfy (/= ',')
  
quotedValue :: Parser String
quotedValue = do
  quote <- char '\"' <|> char '\''
  value <- many1 $ satisfy (/= quote)
  char quote
  return value
    
validNameChar :: Parser Char
validNameChar = alphaNum <|> 
                char '/' <|> 
                char '\\' <|> 
                char '_' <|> 
                char '-' <|> 
                char '@' <|> 
                char '.' <|>
                char '+'

debug :: String -> ()
debug s = unsafePerformIO $! debugM "SASL" s