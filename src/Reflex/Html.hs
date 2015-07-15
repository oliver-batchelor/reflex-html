{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FunctionalDependencies #-}
 
module Reflex.Html
  ( module Reflex.Html.Elements
  , module Reflex.Html.Attributes
  , module Reflex.Html.Events  
  , module Reflex.Html.Input
  , module Reflex.Html.KeyCodes
  
  , MonadAppHost
  
  , runHtml
  , withCss
  
  , HtmlT
  
  ) where

import Reflex.Html.Internal.Host
  
import Reflex.Html.Elements
import Reflex.Html.Attributes
import Reflex.Html.Events
import Reflex.Html.Input
import Reflex.Html.KeyCodes

import Reflex.Html.Internal.HtmlT

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
  
  

withCss :: MonadAppHost t m => ByteString -> HtmlT m ()
withCss css = style_ mempty $ text (B.unpack css)