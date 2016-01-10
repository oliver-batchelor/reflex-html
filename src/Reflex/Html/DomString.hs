module Reflex.Html.DomString
  ( module Data.Text
  , DomString
  , domShow
  ) where


import Data.String
import Data.Text

type DomString = Text


domShow :: Show a => a -> DomString
domShow = fromString . show

