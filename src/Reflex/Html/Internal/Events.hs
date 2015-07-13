module Reflex.Html.Internal.Events where

import Reflex.Html.Internal.HtmlT
import Reflex.Html.Internal.Host


import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Event as Dom
import qualified GHCJS.DOM.EventM as Dom
import qualified GHCJS.DOM.UIEvent as Dom
import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.DOMWindow as Dom



import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

type KeyCode = Int
data EventFlag = StopPropagation | PreventDefault 


data Events t = Events 
  { _on_keypress :: Event t KeyCode
  , _on_keydown  :: Event t KeyCode
  , _on_keyup    :: Event t KeyCode
  , _on_scrolled :: Event t Int
  , _on_clicked  :: Event t ()    
  }


bindEvents :: MonadAppHost t m => Dom.Element -> m (Events t)
bindEvents dom = Events
      <$> keypressEvent_ [] dom
      <*> keydownEvent_ [] dom
      <*> keyupEvent_ [] dom
      <*> scrolledEvent_ [] dom
      <*> clickedEvent_ [] dom 
     
  
  
type EventCallback e event = (e -> Dom.EventM event e () -> IO (IO ()))
 
domEvent :: (MonadAppHost t m, Dom.IsEvent event, Dom.IsElement e) 
  => EventCallback e event -> Dom.EventM event e a -> [EventFlag] -> e -> m (Event t a)
domEvent elementOnevent getValue  flags element = domEventMaybe elementOnevent (liftM Just getValue) flags element


domEventMaybe :: (MonadAppHost t m, Dom.IsEvent event, Dom.IsElement e) => 
  EventCallback e event -> Dom.EventM event e (Maybe a) -> [EventFlag] -> e -> m (Event t a)
domEventMaybe  elementOnevent getValue flags element = do
  fire <- getAsyncFire
  e <- newEventWithTrigger $ \et -> do
        unsubscribe <- liftIO $ elementOnevent element $  do
          mv <-  getValue
          mapM_ applyFlag flags
          
          forM_ mv $ \v -> liftIO $ fire [et :=> v]
        return $ liftIO $ do
          unsubscribe
          
  unless (null flags) $ 
    performEvent_ $ pure () <$ e  
         
  return $!  e
 
 
 
applyFlag :: Dom.IsEvent e => EventFlag -> Dom.EventM e t ()
applyFlag StopPropagation = Dom.stopPropagation
applyFlag PreventDefault = Dom.preventDefault
  

  
--Raw event bindings
clickedEvent_ :: (MonadAppHost t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
clickedEvent_ = domEvent Dom.elementOnclick (return ())

keypressEvent_ :: (MonadAppHost t m) => [EventFlag] -> Dom.Element -> m (Event t Int) 
keypressEvent_ = domEvent Dom.elementOnkeypress  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)

keydownEvent_ :: (MonadAppHost t m) => [EventFlag] -> Dom.Element  -> m (Event t Int)
keydownEvent_ = domEvent Dom.elementOnkeydown  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)

keyupEvent_ :: (MonadAppHost t m) => [EventFlag] -> Dom.Element  -> m (Event t Int)
keyupEvent_ = domEvent Dom.elementOnkeyup  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)

scrolledEvent_ :: (MonadAppHost t m) => [EventFlag] -> Dom.Element -> m (Event t Int)
scrolledEvent_ flags e = domEvent Dom.elementOnscroll (liftIO $ Dom.elementGetScrollTop e) flags e
  
blurEvent_ :: (MonadAppHost t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
blurEvent_ = domEvent Dom.elementOnblur (return ())
 
focusEvent_ :: (MonadAppHost t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
focusEvent_ = domEvent Dom.elementOnfocus (return ()) 


changeEvent_ :: (MonadAppHost t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
changeEvent_ = domEvent Dom.elementOnchange (return ()) 

mouseLocal :: Dom.Element -> Dom.UIEvent -> IO (Int, Int)
mouseLocal e event = do
  x <- Dom.uiEventGetLayerX event
  y <- Dom.uiEventGetLayerY event

  ex <- Dom.elementGetOffsetLeft e
  ey <- Dom.elementGetOffsetTop e
  return (x - ex, y - ey)
  
  
-- mouseMove_ :: MonadAppHost t m => Dom.Element ->  m (Event t (Int, Int))
-- mouseMove_ e = wrapDomEvent e Dom.elementOnkeypress (liftIO . mouseLocal e =<< Dom.event)

-- Events on the Window level

-- askWindow :: (MonadIO m, HasDocument m) => m Dom.DOMWindow
-- askWindow =  do 
--   (Just window) <- askDocument >>= liftIO . Dom.documentGetDefaultView 
--   return window 
-- 
-- window_keydownEvent :: (MonadAppHost t m) => m (Event t Int)
-- window_keydownEvent = askWindow >>= \e -> wrapDomEvent e Dom.domWindowOnkeydown  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)  
  