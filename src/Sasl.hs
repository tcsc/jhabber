module Sasl ( AuthInfo(None),
              AuthResponse(..),
              AuthResult,
              newAuthInfo, 
              challenge,
              checkResponse,
              checkCredentials,
              getUid,
              isAuthenticated,
              nsSasl ) where

import Control.Monad.Error  
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.ByteString.Lazy.UTF8 as LazyUtf8
import Data.ByteString.Internal(w2c, c2w)
import Data.Binary(put)
import Data.Binary.Put(runPut)
import Data.Char
import Data.Digest.Pure.MD5
import Data.Hex
import Data.List
import Data.Maybe(fromMaybe)
import System.Random
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Error
import System.Log.Logger(debugM)
import System.IO.Unsafe

import TextUtils

data DigestState = Uninitialised
                 | Unauthenticated
                 | Pending
                 | Authenticated
                 deriving (Eq,Show)

data AuthResponse = NeedsAuthentication
                  | Challenge String
                  | Success
                  deriving (Show, Eq)

data AuthInfo = None
              | Basic String 
              | Digest { digRealm :: !String,
                         digNonce :: !String,
                         digUid :: !String,
                         digCNonce :: !String,
                         digResponse :: !String,
                         digUri :: !String,
                         digNonceCount :: !String,
                         digQop :: !String,
                         digAuthZId :: !String,
                         digState :: !DigestState }
              deriving (Show)
              
data AuthFailure = UnsupportedMechanism
                 | ProtocolFailure
                 | Mismatch
                 | Unexpected
                 deriving (Read, Show)
                 
instance Error AuthFailure where
  strMsg x = read x

type AuthResult = Either AuthFailure

nsSasl :: String
nsSasl = "urn:ietf:params:xml:ns:xmpp-sasl"

isAuthenticated :: AuthInfo -> Bool
isAuthenticated (Digest _ _ _ _ _ _ _ _ _ state) = 
  if state == Authenticated 
    then True 
    else False
isAuthenticated _ = False

newAuthInfo :: String -> String -> IO (Maybe AuthInfo)
newAuthInfo "DIGEST-MD5" realm = do
  g <- newStdGen
  let n = hex . map chr . take 16 $ (randomRs (0, 255) g :: [Int])
  return $ Just (newDigest realm n)
newAuthInfo _ _ = return Nothing

newDigest :: String -> String -> AuthInfo
newDigest realm nonce = Digest realm nonce "" "" "" "" "" "" "" Uninitialised
  
challenge :: AuthInfo -> String
challenge (Digest realm nonce _ _ _ _ _ _ _ _) = 
  "realm=\"" ++ realm ++ "\"," ++ 
  "nonce=\"" ++ nonce ++ "\"," ++ 
  "qop=\"auth\"," ++ 
  "charset=\"utf-8\"," ++
  "algorithm=\"md5-sess\""

{-| 
  Parses a client response, returning Nothing if the parse fails. The 
  definition of "Failure" in this case also includes  having the values in the
  parsed response not match up to the expected values in the AuthInfo block
 -}  
checkResponse :: AuthInfo -> String -> AuthResult (AuthInfo, AuthResponse)
checkResponse auth@(Digest _ _ _ _ _ _ _ _ _ state) r = do
  case state of
    Uninitialised -> digestStage1 auth r
    Pending -> digestStage2 auth
    _ -> Left Unexpected 
checkResponse authInfo r = Left UnsupportedMechanism

checkCredentials :: AuthInfo -> String -> String -> AuthResult (AuthInfo, AuthResponse)
checkCredentials auth@(Digest _ _ _ _ _ _ _ _ _ _) uid pwd =
  let clientDigest = Utf8.fromString $ digResponse auth
      localDigest = calcDigest auth uid pwd
      response = genResponse auth uid pwd
  in if clientDigest == localDigest 
    then let auth' = auth { digState = Pending }
             ch = "rspauth=" ++ (Utf8.toString response)
    in Right (auth', Challenge ch)
  else 
    Left Mismatch
  
checkCredentials a uid pwd = Left UnsupportedMechanism

lookupItem :: String -> [(String,String)] -> AuthResult String
lookupItem n ps = case lookup n ps of
  Just v -> Right v
  Nothing -> Left ProtocolFailure

digestStage1 :: AuthInfo -> String -> AuthResult (AuthInfo, AuthResponse)
digestStage1 auth@(Digest _ _ _ _ _ _ _ _ _ _) r = do  
  pairs <- parsePairs r
  nonce <- lookupItem "nonce" pairs
  realm <- lookupItem "realm" pairs
  uri <- lookupItem "digest-uri" pairs
  if (nonce /= (digNonce auth)) || (realm /= (digRealm auth)) 
    then Left Mismatch
    else do
      response <- lookupItem "response" pairs
      clientNonce <- lookupItem "cnonce" pairs
      uid <- lookupItem "username" pairs
      nc <- lookupItem "nc" pairs
      qop <- lookupItem "qop" pairs
      let authzid = either (\_ -> "") id $ lookupItem "authzid" pairs 
      let auth' = auth { digUid = uid,
                         digCNonce = clientNonce,
                         digResponse = response,
                         digNonceCount = nc,
                         digAuthZId = authzid,
                         digUri = uri,
                         digQop = qop,
                         digState = Unauthenticated }
      Right (auth', NeedsAuthentication)
                    
digestStage2 :: AuthInfo -> AuthResult (AuthInfo, AuthResponse)
digestStage2 auth =
  let auth' = auth { digState = Authenticated } in
  Right (auth', Success)

{-
needsAuthentication :: AuthInfo -> Bool
needsAuthentication auth@(Digest _ _ _ _ _ _ _ _ _ state) =
  case state of 
    Unauthenticated -> True
    _ -> False

needsAuthentication authInfo = False
-}    

{-
Let { a, b, ... } be the concatenation of the octet strings a, b, ...

Let H(s) be the 16 octet MD5 hash [RFC 1321] of the octet string s.

Let KD(k, s) be H({k, ":", s}), i.e., the 16 octet hash of the string
k, a colon and the string s.

Let HEX(n) be the representation of the 16 octet MD5 hash n as a
string of 32 hex digits (with alphabetic characters always in lower
case, since MD5 is case sensitive).

Let HMAC(k, s) be the 16 octet HMAC-MD5 [RFC 2104] of the octet
string s using the octet string k as a key.

The value of a quoted string constant as an octet string does not
include any terminating null character.

response-value  = 
HEX(
  KD(
    HEX(H(A1)),
    { nonce-value, ":" nc-value, ":", cnonce-value, ":", qop-value, ":", HEX(H(A2)) }))

If authzid is specified, then A1 is

  A1 = { H( { username-value, ":", realm-value, ":", passwd } ), 
    ":", nonce-value, ":", cnonce-value, ":", authzid-value }

If authzid is not specified, then A1 is

   A1 = { H( { username-value, ":", realm-value, ":", passwd } ), 
    ":", nonce-value, ":", cnonce-value }

where
  passwd   = *OCTET
  
  The "username-value", "realm-value" and "passwd" are encoded
  according to the value of the "charset" directive. If "charset=UTF-8"
  is present, and all the characters of either "username-value" or
  "passwd" are in the ISO 8859-1 character set, then it must be
  converted to ISO 8859-1 before being hashed. This is so that
  authentication databases that store the hashed username, realm and
  password (which is common) can be shared compatibly with HTTP, which
  specifies ISO 8859-1. A sample implementation of this conversion is
  in section 8.

  If the "qop" directive's value is "auth", then A2 is:

     A2 = { "AUTHENTICATE:", digest-uri-value }

  If the "qop" value is "auth-int" or "auth-conf" then A2 is:

     A2 = { "AUTHENTICATE:", digest-uri-value, ":00000000000000000000000000000000" }
  
-}

genResponse :: AuthInfo -> String -> String -> BS.ByteString
genResponse auth@(Digest _ _ _ _ _ _ _ _ _ _) uid pwd = 
  let text = digestText auth uid pwd ""
  in hexl . hash $ text

calcDigest :: AuthInfo -> String -> String -> BS.ByteString
calcDigest auth@(Digest _ _ _ _ _ _ _ _ _ _) uid pwd = 
  let text = digestText auth uid pwd "AUTHENTICATE"
  in hexl . hash $ text
        
digestText :: AuthInfo -> String -> String -> String -> BS.ByteString
digestText auth@(Digest realm nonce _ cnonce _ uri ncCount qop authzid _) uid pwd n = 
  let a1 = digestPartA1 realm uid pwd nonce cnonce authzid
      a2 = digestPartA2 n uri
      mid = Utf8.fromString $ intercalate ":" [nonce, ncCount, cnonce, qop]
  in BS.intercalate ((BS.singleton . c2w) ':') [a1, mid, a2]
  
digestPartA1 :: String -> String -> String -> String -> String -> String -> BS.ByteString
digestPartA1 realm uid pwd nonce cnonce authzid = 
  let secret = digestSecret realm uid pwd
      a1b = digestPartA1B nonce cnonce authzid
  in hexl . hash $ BS.intercalate ((BS.singleton . c2w) ':') [secret, a1b] 

digestPartA1B :: String -> String -> String -> BS.ByteString
digestPartA1B nonce cnonce authzid = 
  Utf8.fromString $ intercalate ":" $ if authzid == "" 
                      then [nonce, cnonce]
                      else [nonce, cnonce, authzid]

digestPartA2 :: String -> String -> BS.ByteString
digestPartA2 n uri = hexl . hashL . LazyUtf8.fromString $ intercalate ":" [n, uri]
                      
digestSecret :: String -> String -> String -> BS.ByteString
digestSecret realm uid pwd = hashS $ intercalate ":" [uid, realm, pwd]

hexl :: BS.ByteString -> BS.ByteString
hexl = BS.map (c2w . toLower . w2c) . hex

hash :: BS.ByteString -> BS.ByteString
hash bs = hashL $ LazyBS.fromChunks [bs]

hashL :: LazyBS.ByteString -> BS.ByteString
hashL bs = (BS.concat . LazyBS.toChunks . runPut . put . md5) bs

hashS :: String -> BS.ByteString
hashS = hashL . LazyUtf8.fromString

getUid :: AuthInfo -> String
getUid auth@(Digest _ _ uid _ _ _ _ _ _ _) = uid

parsePairs :: String -> AuthResult [(String, String)]
parsePairs s = case parse pairs "" s of
                 Right r -> Right r
                 _ -> Left ProtocolFailure
  
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
--debug s = unsafePerformIO $! debugM "SASL" s
debug s = unsafePerformIO $! putStr s