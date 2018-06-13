module Builder.TH.Attribute where

import Prelude
import Data.Char
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Reflex.Dom (Namespace)
import Builder.Element

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Data.Default

data AttributeConfig = A String TypeQ ExpQ -- | M Text TypeQ ExpQ


mkConName :: String -> String
mkConName (c:cs) | isLetter c = toUpper c : map replace cs
               | otherwise  = error "mkAttrs: first letter of attribute must be a letter"
  where
    replace '-' = '_'
    replace c   = c


mkAttrs :: String -> [AttributeConfig] -> Q [Dec]
mkAttrs setName attrs = do
  a <- newName "a"
  
  constructors <- traverse genCons attrs
  return [DataD [] typeName [PlainTV a] Nothing (constructors) []]

  where

    genCons (A attrName attrType _) = do 
      t <- attrType
      return $ GadtC [conName] [] (AppT (ConT typeName) t)
        where conName = mkName (mkConName attrName)

    typeName = mkName setName


strA :: String -> AttributeConfig
strA name = A name [t| Text |] [| Just |]

intA :: String -> AttributeConfig
intA name = A name [t| Int |] [| Just . T.pack . show |]

floatA :: String -> AttributeConfig
floatA name = A name [t| Float |] [| Just . T.pack . show |]


  
boolA :: String -> AttributeConfig
boolA name = A name [t| Bool |] [| \b -> if b then Just "" else Nothing |]

ifA :: String -> (String, String) -> AttributeConfig
ifA name (trueVal, falseVal) = A name [t| Bool |] [| \b -> Just (if b then $trueLit else $falseLit) |] where
  trueLit = pure $ LitE (StringL trueVal)
  falseLit = pure $ LitE (StringL falseVal)
  
listA :: Char -> AttributeConfig -> AttributeConfig
listA sep (A name t f) = A name [t| [$t] |] [| Just . intercalate [$sepLit] . map $f |] where
  sepLit = pure $ LitE (CharL sep)

spaceSep :: AttributeConfig -> AttributeConfig
spaceSep = listA ' '

commaSep :: AttributeConfig -> AttributeConfig
commaSep = listA ','

semicolonSep :: AttributeConfig -> AttributeConfig
semicolonSep = listA ';'


styleA :: String -> AttributeConfig
styleA name = semicolonSep $ A name [t| (Text, Text) |] [| \(style, value) -> style <> ":" <> value |]
