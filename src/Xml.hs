module Xml ( XmlElement(..),
             XmlAttribute(XmlAttribute),
             formatElement,
             formatElements,
             formatShortElement,
             newElement,
             getAttribute,
             getChild,
             getChildText,
             getNamedChildren,
             getNamedChild,
             selectChildren ) where


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

-- | get the n'th child of the xml element, where n is a 0-based index
getChild :: XmlElement -> Int -> Maybe XmlElement
getChild e@(XmlElement _ _ _ children) n =
  if (length children) > n then
    Just $ children !! n
  else
    Nothing

selectChildren :: (XmlElement -> Bool) -> XmlElement -> [XmlElement]
selectChildren f xml = filter f (children xml)

getNamedChildren :: String -> String -> XmlElement -> [XmlElement]
getNamedChildren ns n (XmlElement _ _ _ cs) =  filter (checkName ns n) cs
  where
    checkName :: String -> String -> XmlElement -> Bool
    checkName ns n (XmlElement cns cn _ _) = (ns == cns) && (n == cn)
    checkName _ _ _ = False

-- |
getNamedChild :: String -> String -> XmlElement -> Maybe XmlElement
getNamedChild ns n xml =
  case getNamedChildren ns n xml of
    [] -> Nothing
    c -> Just $ head c

-- | Fetches the text of the first child of the element, if (and only if) that
--   child is a text node
getChildText :: XmlElement -> Maybe String
getChildText xml = do
  child <- getChild xml 0
  case child of
    XmlText s -> return s
    otherwise -> Nothing

formatElements :: Bool -> [XmlElement] -> String
formatElements short elems = (concat $ map (formatElement short) elems)

formatElement :: Bool -> XmlElement -> String
formatElement short e@(XmlElement _ _ attribs subs) =
  let fullName = qualifiedName e in
    "<" ++ fullName ++
      formatAttributes attribs ++
      if short then ">"
        else case subs of
              [] -> "/>"
              _ -> ">" ++ (formatElements False subs) ++ "</" ++ fullName ++ ">"
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
