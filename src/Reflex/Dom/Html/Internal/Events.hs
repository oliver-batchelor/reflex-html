{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction, InstanceSigs #-}

module Reflex.Dom.Html.Internal.Events where

import Reflex
import Reflex.Dom

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


bindEvents :: MonadWidget t m => Dom.Element -> m (Events t)
bindEvents dom = Events
      <$> keypressEvent_ [] dom
      <*> keydownEvent_ [] dom
      <*> keyupEvent_ [] dom
      <*> scrolledEvent_ [] dom
      <*> clickedEvent_ [] dom 
     
wrapEvent :: (MonadWidget t m, Dom.IsEvent event, Dom.IsElement el) => (el -> Dom.EventM event el () -> IO (IO ())) -> Dom.EventM event el a -> [EventFlag] -> el -> m (Event t a)
wrapEvent onEvent getResult flags e = do
  event <- wrapDomEvent e onEvent (mapM_ applyFlag flags >> getResult)  
  unless (null flags) $ performEvent_ $ ffor event $ const $ return ()
  return event
  
 
applyFlag :: Dom.IsEvent e => EventFlag -> Dom.EventM e t ()
applyFlag StopPropagation = Dom.stopPropagation
applyFlag PreventDefault = Dom.preventDefault
  

  
--Raw event bindings
clickedEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
clickedEvent_ = wrapEvent Dom.elementOnclick (return ())

keypressEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element -> m (Event t Int) 
keypressEvent_ = wrapEvent Dom.elementOnkeypress  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)

keydownEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element  -> m (Event t Int)
keydownEvent_ = wrapEvent Dom.elementOnkeydown  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)

keyupEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element  -> m (Event t Int)
keyupEvent_ = wrapEvent Dom.elementOnkeyup  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)

scrolledEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element -> m (Event t Int)
scrolledEvent_ flags e = wrapEvent Dom.elementOnscroll (liftIO $ Dom.elementGetScrollTop e) flags e
  
blurEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
blurEvent_ = wrapEvent Dom.elementOnblur (return ())
 
focusEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
focusEvent_ = wrapEvent Dom.elementOnfocus (return ()) 


changeEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
changeEvent_ = wrapEvent Dom.elementOnchange (return ()) 

mouseLocal :: Dom.Element -> Dom.UIEvent -> IO (Int, Int)
mouseLocal e event = do
  x <- Dom.uiEventGetLayerX event
  y <- Dom.uiEventGetLayerY event

  ex <- Dom.elementGetOffsetLeft e
  ey <- Dom.elementGetOffsetTop e
  return (x - ex, y - ey)
  
  
mouseMove_ :: MonadWidget t m => Dom.Element ->  m (Event t (Int, Int))
mouseMove_ e = wrapDomEvent e Dom.elementOnkeypress (liftIO . mouseLocal e =<< Dom.event)

-- Events on the Window level

askWindow :: (MonadIO m, HasDocument m) => m Dom.DOMWindow
askWindow =  do 
  (Just window) <- askDocument >>= liftIO . Dom.documentGetDefaultView 
  return window 

window_keydownEvent :: (MonadWidget t m) => m (Event t Int)
window_keydownEvent = askWindow >>= \e -> wrapDomEvent e Dom.domWindowOnkeydown  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)  
  