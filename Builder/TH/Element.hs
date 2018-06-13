module Builder.TH.Element where

import Prelude
import Data.Char
import Data.Monoid
import Data.Text (Text, unpack)

import Reflex.Dom (Namespace)
import Builder.Element

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Data.Default

data ElementConfig = E String | C String


mkElems ::  Maybe String -> TypeQ -> [ElementConfig] -> Q [Dec]
mkElems ns attrs elems = concat <$> traverse gen elems  where
    gen (E name) = concat <$> traverse (\f -> f attrs ns name) [mkElem', mkElem_, mkElem]
    gen (C name) = mkChild_ attrs ns name

helper :: String -> Type -> Exp -> [Dec]
helper elemName typ f = [ SigD n typ, ValD (VarP n) (NormalB f) [], PragmaD (InlineP n Inline FunLike AllPhases) ]
  where n = mkName elemName

mkElem' :: TypeQ -> Maybe String -> String -> Q [Dec]
mkElem' attrs ns elemName = helper (elemName <> "'")
    <$> [t| Elem' $attrs |]
    <*> [| makeElem' ns elemName  |]


mkChild_ :: TypeQ -> Maybe String -> String -> Q [Dec]
mkChild_ attrs ns elemName = helper (elemName <> "_")
  <$> [t| Child_ $attrs |]
  <*> [| makeChild_ ns elemName  |]

mkElem :: TypeQ ->  Maybe String -> String -> Q [Dec]
mkElem attrs ns elemName = helper elemName
  <$> [t| Elem $attrs |]
  <*> [| makeElem ns elemName  |]

mkElem_ :: TypeQ ->  Maybe String -> String -> Q [Dec]
mkElem_ attrs ns elemName = helper (elemName <> "_")
  <$> [t| Elem_ $attrs |]
  <*> [| makeElem_ ns elemName  |]
