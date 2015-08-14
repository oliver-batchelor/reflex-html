module Reflex.Html.Internal.Events 
  ( module Reflex.Html.Internal.Events
  , module Reflex.Html.Internal.DomEvents   
  ) where

import Reflex.Html.Internal.HtmlT
import Reflex.Html.Internal.Host
import Reflex.Html.Internal.DomEvents

import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Event as Dom
import qualified GHCJS.DOM.EventM as Dom
import qualified GHCJS.DOM.UIEvent as Dom
import qualified GHCJS.DOM.MouseEvent as Dom
import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.DOMWindow as Dom


import Data.Functor.Misc
import Data.Bitraversable

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Lens

data EventFlag = StopPropagation | PreventDefault 


type Events t = EventSelector t (WrapArg EventResult EventName)

bindEvents :: MonadAppHost t m => Dom.Element -> HtmlT m (Events t)
bindEvents dom = lift $ wrapDomEventsMaybe dom $ defaultDomEventHandler dom
     
 
type EventsHandler f e en  = (forall en. EventName en -> Dom.EventM (EventType en) e (Maybe (f en)))

wrapDomEventsMaybe :: (MonadAppHost t m, Dom.IsElement e) => e -> EventsHandler f e en -> m (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe element handlers = do
  postAsync <- getPostAsync
  e <- newFanEventWithTrigger $ \(WrapArg en) et -> do
        unsubscribe <- liftIO $ (onEventName en) element $ do
          mv <- handlers en
          forM_ mv $ \v -> liftIO $ postAsync $ pure [et :=> v]
        return . liftIO $ unsubscribe
  return $! e


type EventHandler e event =  (e -> Dom.EventM event e () -> IO (IO ()))

wrapDomEvent :: (MonadAppHost t m, Dom.IsElement e) => e -> EventHandler  e event -> Dom.EventM event e a -> m (Event t a)
wrapDomEvent element elementOnevent getValue = wrapDomEventMaybe element elementOnevent $ liftM Just getValue

wrapDomEventMaybe :: (MonadAppHost t m, Dom.IsElement e) => e -> EventHandler e event -> Dom.EventM event e (Maybe a) -> m (Event t a)
wrapDomEventMaybe element elementOnevent getValue = do
  postAsync <- getPostAsync

  e <- newEventWithTrigger $ \et -> do
        unsubscribe <- liftIO $  elementOnevent element $ do
          mv <-  getValue
          forM_ mv $ \v -> liftIO $ postAsync $ pure [et :=> v]
        return $ liftIO $  unsubscribe
  return $! e  
  
  
getKeyEvent :: Dom.EventM Dom.UIEvent e KeyCode
getKeyEvent = do
  e <- Dom.event
  liftIO $ do
    which <- Dom.uiEventGetWhich e
    if which /= 0 then return which else do
      charCode <- Dom.uiEventGetCharCode e
      if charCode /= 0 then return charCode else
        Dom.uiEventGetKeyCode e

mouseEvent ::Dom.IsElement e => e -> Dom.EventM Dom.MouseEvent e (Int, Int)
mouseEvent e = do
  ev <- Dom.event
  liftIO $ bisequence (Dom.mouseEventGetX ev, Dom.mouseEventGetY ev)
  
  

defaultDomEventHandler :: Dom.IsElement e => e -> EventName en -> Dom.EventM (EventType en) e (Maybe (EventResult en))
defaultDomEventHandler e evt = liftM (Just . EventResult) $ case evt of
  Click         -> return ()
  Dblclick      -> return ()
  Keypress      -> getKeyEvent
  Keydown       -> {-getKeyEvent-} Dom.event >>= liftIO . Dom.uiEventGetKeyCode
  
  Scroll        -> liftIO $ Dom.elementGetScrollTop e
  Mousemove     -> mouseEvent e
  Mouseup       -> mouseEvent e
  Mousedown     -> mouseEvent e
  Mouseenter    -> return ()
  Mouseleave    -> return ()
  Focus         -> return ()
  Blur          -> return ()  
  Input         -> return ()
  _             -> error "Missing handler in defaultDomEventHandler"