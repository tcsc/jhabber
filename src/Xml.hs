module Xml ( XmlElement(..),
             XmlAttribute(XmlAttribute),
             formatElement,
             formatElements,
             formatShortElement,
             newElement,
             getAttribute,
             getChild ) where


import Data.List

data XmlElement = XmlElement { elemNamespace :: !String,
                               elemName :: !String,
                               attributes :: ![XmlAttribute],
                               children :: ![XmlElement] }
                | XmlProcessingInstruction String
                | XmlText String
                deriving (Show, Eq)

data XmlAttribute = XmlAttribute { attrNamespace :: String,
                                   attrName :: String,
                                   attrValue :: String }
                                   deriving (Show, Eq)

newElement :: String -> XmlElement
newElement n = XmlElement "" n [] []

-- | Fetches an attribute from the XML element
getAttribute :: String -> String -> XmlElement -> Maybe String
getAttribute nameSpace name (XmlElement _ _ attributes _) = do
  attrib <- find (\(XmlAttribute ns n _) ->ns == nameSpace && n == name) attributes
  return $ attrValue attrib

getChild :: XmlElement -> Int -> Maybe XmlElement
getChild e@(XmlElement _ _ _ children) n =
  if (length children) > n then
    Just $ children !! n
  else
    Nothing

formatElements :: Bool -> [XmlElement] -> String
formatElements short elems = (concat $ map (formatElement short) elems)

formatElement :: Bool -> XmlElement -> String
formatElement short e@(XmlElement _ _ attribs subs) =
  let fullName = qualifiedName e in
    "<" ++ fullName ++
      formatAttributes attribs ++ ">" ++
      if short
        then ""
      else (formatElements False subs) ++ "</" ++ fullName ++ ">"
formatElement _ (XmlText s) = s

formatShortElement :: XmlElement -> String
formatShortElement = formatElement True

qualifiedName :: XmlElement -> String
qualifiedName (XmlElement ns n _ _) =
  if ns == "" then n
  else ns ++ ":" ++ n

formatAttributes :: [XmlAttribute] -> String
formatAttributes [] = ""
formatAttributes attrs = concat $
  map (\(XmlAttribute ns name value) ->
        let fullname = if ns == "" then name else ns ++ ":" ++ name
        in " " ++ fullname ++ "=\"" ++ value ++ "\"") attrs
