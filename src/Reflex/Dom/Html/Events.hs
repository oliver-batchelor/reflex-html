module Reflex.Dom.Html.Events 
  ( module Reflex.Dom.Html.Events
  , EventFlag(..)
  ) 
  where

import Reflex.Dom hiding (Attributes)
import Data.Functor

import Reflex.Dom.Html.Internal.Events
import Reflex.Dom.Html.Internal.Element
 

-- Simple event accessors
clicked :: IsElement e  => e tag t -> Event t ()
clicked = _element_clicked . toElement

keypress :: IsElement e => e tag t -> Event t KeyCode
keypress = _element_keypress . toElement

keydown :: IsElement e => e tag t -> Event t KeyCode
keydown = _element_keydown . toElement

keyup :: IsElement e => e tag t -> Event t KeyCode
keyup = _element_keyup . toElement

scrolled :: IsElement e => e tag t -> Event t Int
scrolled = _element_scrolled . toElement


 

-- Event binders

clickedEvent = liftEvent clickedEvent_
keypressEvent = liftEvent keypressEvent_
keydownEvent = liftEvent keydownEvent_
keyupEvent = liftEvent keyupEvent_
scrolledEvent = liftEvent scrolledEvent_
focusEvent = liftEvent focusEvent_
blurEvent = liftEvent blurEvent_


-- Convenience bindings (used by Input)
holdFocus :: (MonadWidget t m, IsElement e) => e tag t -> m (Dynamic t Bool)
holdFocus e = do
  eFocus <- focusEvent [] e
  eBlur <- blurEvent [] e
  holdDyn False $ leftmost [True <$ eFocus, False <$ eBlur]
