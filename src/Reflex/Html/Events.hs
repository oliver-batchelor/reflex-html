module Reflex.Html.Events 
  ( module Reflex.Html.Events
  , module Reflex.Html.Internal.ElementEvents 
  , EventFlag(..)
  ) 
  where

import Data.Functor

import Reflex.Html.Internal.Host
import Reflex.Html.Internal.HtmlT

import Reflex.Html.Internal.Events
import Reflex.Html.Internal.ElementEvents

import Reflex.Html.Internal.Element
 

 
-- Event accessors
clicked = domEvent Click
focused = domEvent Focus
blur = domEvent Blur


-- Convenience bindings (used by Input)
holdFocus :: (HasDomEvent t e, MonadAppHost t m) => e -> HtmlT m (Dynamic t Bool)
holdFocus e = holdDyn False $ leftmost [True <$ focused e, False <$ blur e]


