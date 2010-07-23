module TextUtils where
  
import Control.Monad
import Text.Read

readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of 
  [(x, "")] -> return x
  _ -> fail "parse failure"
  