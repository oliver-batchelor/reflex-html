{-# LANGUAGE ScopedTypeVariables, RankNTypes, TupleSections, GADTs, TemplateHaskell, ConstraintKinds, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Reflex.Html.Render  where


import qualified GHCJS.DOM as Dom
import qualified GHCJS.DOM.Node as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.EventM as Dom

import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.RSS.Strict

import Control.Concurrent.Chan
import Control.Lens

import Data.Dependent.Sum
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Foldable
import Data.Functor.Misc

import Reflex
import Reflex.Host.Class

import Data.IORef
import Data.Unsafe.Tag
import Reflex.Html.Event

data Env t = Env
  { envDoc    :: Dom.Document
  , envParent :: Dom.Node
  , envChan   :: Chan (DSum (EventTrigger t))
  }


type RenderM t = (MonadIO (HostFrame t), ReflexHost t)
newtype Render t a = Render { unRender :: RSST (Env t)  (DMap Tag) () (HostFrame t) a }

type EventHandler e event =  (e -> Dom.EventM e event () -> IO (IO ()))
type EventsHandler f e en  = (forall en. EventName en -> Dom.EventM  e (EventType en)  (Maybe (f en)))


deriving instance ReflexHost t => Functor (Render t)
deriving instance ReflexHost t => Applicative (Render t)
deriving instance ReflexHost t => Monad (Render t)
deriving instance ReflexHost t => MonadReader (Env t) (Render t)
deriving instance ReflexHost t => MonadWriter (DMap Tag) (Render t)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (Render t)
deriving instance ReflexHost t => MonadFix (Render t)

type RenderEvent t = Event t (DMap Tag)

instance ReflexHost t => MonadReflexCreateTrigger t (Render t) where
  newEventWithTrigger = Render . lift . newEventWithTrigger
  newFanEventWithTrigger initializer = Render $ lift $ newFanEventWithTrigger initializer

instance ReflexHost t => MonadSample t (Render t) where
  sample = Render . lift . sample

instance ReflexHost t => MonadHold t (Render t) where
  hold a0 = Render . lift . hold a0


askDocument :: RenderM t => Render t Dom.Document
askDocument = asks envDoc

askPostAsync :: RenderM t => Render t (DSum (EventTrigger t) -> IO ())
askPostAsync = post <$> asks envChan
  where post chan tv = writeChan chan tv


withParent :: RenderM t =>  (Dom.Node -> Dom.Document -> Render t a) -> Render t a
withParent f = do
  env <- ask
  f (envParent env) (envDoc env)


renderEmptyElement :: RenderM t => String -> String -> Map String String -> Render t Dom.Element
renderEmptyElement ns tag attrs = withParent $ \parent doc -> liftIO $ do
  Just dom <- Dom.createElementNS doc (Just ns) (Just tag)
  imapM_ (Dom.setAttribute dom) attrs
  void $ Dom.appendChild parent $ Just dom
  return $ Dom.castToElement dom


renderText :: RenderM t => String -> Render t Dom.Text
renderText str = withParent $ \parent doc -> liftIO $ do
  Just dom <- Dom.createTextNode doc str
  void $ Dom.appendChild parent $ Just dom
  return dom



renderElement :: RenderM t => String -> String -> Map String String -> Render t a -> Render t (Dom.Element, a)
renderElement ns tag attrs child = do
  dom <- renderEmptyElement ns tag attrs
  a <- local (reParent (Dom.toNode dom)) child
  return (dom, a)

  where
    reParent dom e = e { envParent = dom }


runRender :: RenderM t => Render t a -> Chan (DSum (EventTrigger t)) -> Dom.Document -> Dom.Node -> (HostFrame t) a
runRender (Render render) chan doc root = do
  (a, _) <- evalRSST render (Env doc root chan) ()
  return a


postTriggerRef :: RenderM t => a -> IORef (Maybe (EventTrigger t a)) -> Render t ()
postTriggerRef a ref = do
  postAsync <- askPostAsync
  liftIO $ readIORef ref >>= traverse_ (\t -> postAsync (t :=> a))

postResult :: RenderM t => IORef (Maybe (EventTrigger t (DMap Tag))) -> Render t () -> Render t ()
postResult ref render = do
  (_, result) <- listen render
  postTriggerRef result ref


renderBody ::  Render Spider () -> IO ()
renderBody render = Dom.runWebGUI $ \webView -> do
    Dom.enableInspector webView
    Just doc <- Dom.webViewGetDomDocument webView
    Just body <- Dom.getBody doc

    chan <- liftIO newChan
    runSpiderHost $ runHostFrame $
      runRender render chan doc (Dom.toNode body)


returnRender :: RenderM t => Tag a -> Render t a -> Render t ()
returnRender t r = r >>= tell . DMap.singleton t


wrapDomEvent :: (RenderM t, Dom.IsElement e) => e -> EventHandler  e event -> Dom.EventM e event a -> Render t (Event t a)
wrapDomEvent element elementOnevent getValue = wrapDomEventMaybe element elementOnevent $ Just <$> getValue

wrapDomEventMaybe :: (RenderM t, Dom.IsElement e) => e -> EventHandler e event -> Dom.EventM e event (Maybe a) -> Render t (Event t a)
wrapDomEventMaybe element onEvent getValue = do
  postAsync <- askPostAsync
  newEventWithTrigger $ \et -> onEvent element $
      getValue >>= liftIO . traverse_ (\v -> postAsync (et :=> v))


type Events t = EventSelector t (WrapArg EventResult EventName)

bindEvents :: (RenderM t, Dom.IsElement e) => e -> Render t (Events t)
bindEvents dom = wrapDomEventsMaybe dom (defaultDomEventHandler dom)

wrapDomEventsMaybe :: (RenderM t, Dom.IsElement e) => e -> EventsHandler f e en -> Render t (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe element handlers = do
  postAsync  <- askPostAsync
  newFanEventWithTrigger $ \(WrapArg en) et -> onEventName en element  $ do
      handlers en >>= liftIO . traverse_ (\v -> postAsync (et :=> v))





