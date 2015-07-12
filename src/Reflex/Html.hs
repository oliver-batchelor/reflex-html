{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FunctionalDependencies #-}
 
module Reflex.Dom.Html 
  ( module Reflex.Dom.Html.Elements
  , module Reflex.Dom.Html.Attributes
  , module Reflex.Dom.Html.Events  
  , module Reflex.Dom.Html.Input
  , module Reflex.Dom.Html.KeyCodes
  , module Reflex.Dom
  
  , mainWidget
  , mainWidgetWithCss
  
  , Html
  
  
  ) where

import Reflex.Dom (performEvent, performEvent_, MonadWidget, text, dynText, delay, getPostBuild, list, (=:), display)
import qualified Reflex.Dom as R
import qualified Reflex.Host.Class as R

import Reflex.Dom.Html.Elements
import Reflex.Dom.Html.Attributes
import Reflex.Dom.Html.Events
import Reflex.Dom.Html.Input
import Reflex.Dom.Html.KeyCodes

import Reflex.Dom.Html.Internal.Html
import Reflex.Dom.Html.Internal.Tag

import Data.ByteString (ByteString)

type Impl = R.Widget R.Spider (R.Gui R.Spider (R.WithWebView R.SpiderHost) (R.HostFrame R.Spider))

mainWidget ::  Html Flow Body_ Impl () -> IO ()
mainWidget body = R.mainWidget (runHtml body)


mainWidgetWithCss ::  ByteString -> Html Flow Body_ Impl () -> IO ()
mainWidgetWithCss css body = R.mainWidgetWithCss css (runHtml body)

