module Reflex.Html.Elements 
  ( module Reflex.Html.Elements
  , Element  
  , IsElement (..)
  )
  where

import Reflex.Html.Internal.Element
import Reflex.Html.Internal.Attributes

import Reflex.Html.Internal.HtmlT
import Reflex.Html.Internal.Host

empty :: (MonadAppHost t m) => HtmlT m ()
empty = return ()  

