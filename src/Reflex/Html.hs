{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FunctionalDependencies #-}
 
module Reflex.Html
  ( module Reflex.Html.Elements
  , module Reflex.Html.Attributes
  , module Reflex.Html.Events  
  , module Reflex.Html.Input
  , module Reflex.Html.KeyCodes
  
  , runHtml
  , withCss
  
  , Html
  
  
  ) where

import Reflex.Html.Internal.Host
  
import Reflex.Html.Elements
import Reflex.Html.Attributes
import Reflex.Html.Events
import Reflex.Html.Input
import Reflex.Html.KeyCodes

import Reflex.Html.Internal.HtmlT

import Data.ByteString (ByteString)


runHtml :: HtmlT (AppHost Spider) () -> HtmlT (AppHost Spider) () -> IO ()
runHtml head body = undefined


withCss :: String -> Html m ()
withCss = undefined