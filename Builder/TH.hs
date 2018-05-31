module Builder.TH where

import Prelude
import Data.Monoid
import Data.Text (Text, unpack)

import Reflex.Dom (Namespace)
import Builder.Element

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- mkAttributes ::  Maybe String -> [String] -> Q [Dec]
data ElementType = E String | C String

mkElems ::  Maybe String -> [ElementType] -> Q [Dec]
mkElems ns elems = concat <$> traverse gen elems  where
    gen (E name) = concat <$> traverse (\f -> f ns name) [mkElem', mkElem_, mkElem]
    gen (C name) = mkChild_ ns name

helper :: String -> Type -> Exp -> [Dec]
helper elemName typ f = [ SigD n typ, ValD (VarP n) (NormalB f) [], PragmaD (InlineP n Inline FunLike AllPhases) ]
  where n = mkName elemName

mkElem' :: Maybe String -> String -> Q [Dec]
mkElem' ns elemName = helper (elemName <> "'") <$> [t| Elem' |] <*> [| makeElem' ns elemName  |]

mkChild_ :: Maybe String -> String -> Q [Dec]
mkChild_ ns elemName = helper (elemName <> "_") <$> [t| Child_ |] <*> [| makeChild_ ns elemName  |]

mkElem :: Maybe String -> String -> Q [Dec]
mkElem ns elemName = helper elemName <$> [t| Elem |] <*> [| makeElem ns elemName  |]

mkElem_ :: Maybe String -> String -> Q [Dec]
mkElem_ ns elemName = helper (elemName <> "_") <$> [t| Elem_ |] <*> [| makeElem_ ns elemName  |]
