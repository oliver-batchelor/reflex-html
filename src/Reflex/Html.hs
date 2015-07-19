{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FunctionalDependencies #-}
 
module Reflex.Html
  ( module Reflex.Html.Elements
  , module Reflex.Html.Attributes
  , module Reflex.Html.Events  
  , module Reflex.Html.Input
  , module Reflex.Html.KeyCodes
  , module Reflex.Html.Collection
  , module Reflex.Html.Time

  
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
import Reflex.Html.Collection
import Reflex.Html.Time



import Reflex.Html.Internal.HtmlT

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding
  

withCss :: MonadAppHost t m => ByteString -> HtmlT m ()
withCss css = style_ mempty $ text $ T.unpack (decodeUtf8 css)