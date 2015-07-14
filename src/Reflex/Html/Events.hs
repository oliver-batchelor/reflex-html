module Reflex.Html.Events 
  ( module Reflex.Html.Events
  , EventFlag(..)
  ) 
  where

import Data.Functor

import Reflex.Html.Internal.Host

import Reflex.Html.Internal.Events
import Reflex.Html.Internal.Element
 
 
toEvents :: IsElement e => e -> Events (T e)
toEvents = _element_events . toElement

 
-- Simple event accessors
clicked :: IsElement e  => e -> Event (T e) ()
clicked = _on_clicked . toEvents


keypress :: IsElement e => e -> Event (T e) KeyCode
keypress = _on_keypress . toEvents

keydown :: IsElement e => e-> Event (T e) KeyCode
keydown = _on_keydown . toEvents

keyup :: IsElement e => e -> Event (T e) KeyCode
keyup = _on_keyup . toEvents

scrolled :: IsElement e => e -> Event (T e) Int
scrolled = _on_scrolled . toEvents

  

-- Event binders

clickedEvent = liftEvent clickedEvent_
keypressEvent = liftEvent keypressEvent_
keydownEvent = liftEvent keydownEvent_
keyupEvent = liftEvent keyupEvent_
scrolledEvent = liftEvent scrolledEvent_
focusEvent = liftEvent focusEvent_
blurEvent = liftEvent blurEvent_


-- Convenience bindings (used by Input)
holdFocus :: (MonadAppHost (T e) m, IsElement e) => e -> m (Dynamic (T e) Bool)
holdFocus e = do
  eFocus <- focusEvent [] e
  eBlur <- blurEvent [] e
  holdDyn False $ leftmost [True <$ eFocus, False <$ eBlur]
