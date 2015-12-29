{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables, RankNTypes, TupleSections, GADTs, TemplateHaskell, ConstraintKinds, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, LambdaCase #-}

module Reflex.Html.Render  where

import qualified GHCJS.DOM as Dom
import qualified GHCJS.DOM.Node as Dom
import qualified GHCJS.DOM.Text as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.EventM as Dom

import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Concurrent.Chan
import Control.Lens

import Data.Dependent.Sum
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Foldable
import Data.Functor.Misc

import Reflex
import Reflex.Host.Class

import Data.IORef
import Data.Unsafe.Tag
import Reflex.Html.Event
import Reflex.Monad

import qualified Data.List.NonEmpty as NE
import Data.Semigroup.Applicative

data Env t = Env
  { envDoc    :: Dom.Document
  , envParent :: Dom.Node
  , envChan   :: Chan (DSum (EventTrigger t))
  }


class (ReflexHost t, MonadIO (Host t), MonadSample t (Host t), MonadReflexCreateTrigger t (Host t)) => Renderer t where
  type Host t :: * -> *

instance Renderer Spider where
  type Host Spider = SpiderHost


newtype Render t a = Render { unRender :: StateT [DSum Tag] (Host t) a }

deriving instance Renderer t => Functor (Render t)
deriving instance Renderer t => Applicative (Render t)
deriving instance Renderer t => Monad (Render t)
deriving instance Renderer t => MonadIO (Render t)
deriving instance Renderer t => MonadState [DSum Tag] (Render t)
deriving instance Renderer t => MonadSample t (Render t)


instance Renderer t => MonadReflexCreateTrigger t (Render t) where
  newEventWithTrigger      = Render . lift . newEventWithTrigger
  newFanEventWithTrigger f = Render $ lift $ newFanEventWithTrigger f


newtype Builder t a = Build { unBuild :: ReaderT (Env t) (Render t) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadSample t, MonadReader (Env t), MonadState [DSum Tag])


type EventHandler e event =  (e -> Dom.EventM e event () -> IO (IO ()))
type EventsHandler f e en  = (forall en. EventName en -> Dom.EventM  e (EventType en)  (Maybe (f en)))


askDocument :: Renderer t => Builder t Dom.Document
askDocument = asks envDoc

askPostAsync :: Renderer t => Builder t (DSum (EventTrigger t) -> IO ())
askPostAsync = post <$> asks envChan
  where post chan tv = writeChan chan tv


withParent :: Renderer t =>  (Dom.Node -> Dom.Document -> Builder t a) -> Builder t a
withParent f = do
  env <- ask
  f (envParent env) (envDoc env)


buildEmptyElement :: Renderer t => String -> String -> Map String String -> Builder t Dom.Element
buildEmptyElement ns tag attrs = withParent $ \parent doc -> liftIO $ do
  Just dom <- Dom.createElementNS doc (Just ns) (Just tag)
  imapM_ (Dom.setAttribute dom) attrs
  void $ Dom.appendChild parent $ Just dom
  return $ Dom.castToElement dom


buildText :: Renderer t => String -> Builder t Dom.Text
buildText str = withParent $ \parent doc -> liftIO $ do
  Just dom <- Dom.createTextNode doc str
  void $ Dom.appendChild parent $ Just dom
  return dom

updateText :: Dom.Text -> String -> IO ()
updateText text =  void . Dom.replaceWholeText text


buildElement :: Renderer t => String -> String -> Map String String -> Builder t a -> Builder t (Dom.Element, a)
buildElement ns tag attrs child = do
  dom <- buildEmptyElement ns tag attrs
  a <- local (reParent (Dom.toNode dom)) child
  return (dom, a)

  where
    reParent dom e = e { envParent = dom }



type TriggerRef t a = IORef (Maybe (EventTrigger t a))

postTriggerRef :: Renderer t => a -> TriggerRef t a -> Builder t ()
postTriggerRef a ref = do
  postAsync <- askPostAsync
  liftIO $ readIORef ref >>= traverse_ (\t -> postAsync (t :=> a))


buildBody :: TriggerRef Spider (DMap Tag) -> Builder Spider () -> IO ()
buildBody tr (Build build) = Dom.runWebGUI $ \webView -> do
    Dom.enableInspector webView
    Just doc <- Dom.webViewGetDomDocument webView
    env <- Env doc
      <$> (Dom.toNode . fromJust <$> Dom.getBody doc)
      <*> newChan

    occs <- runSpiderHost $
      execStateT (unRender (runReaderT build env)) []
    return ()

return_ :: MonadState [DSum Tag] m => Tag a -> a -> m ()
return_ t a = tellOcc  (t :=> a)

tellOcc :: MonadState [DSum Tag] m => DSum Tag -> m ()
tellOcc occ = modify (occ:)

wrapDomEvent :: (Renderer t, Dom.IsElement e) => e -> EventHandler  e event -> Dom.EventM e event a -> Builder t (Event t a)
wrapDomEvent element elementOnevent getValue = wrapDomEventMaybe element elementOnevent $ Just <$> getValue

wrapDomEventMaybe :: (Renderer t, Dom.IsElement e) => e -> EventHandler e event -> Dom.EventM e event (Maybe a) -> Builder t (Event t a)
wrapDomEventMaybe element onEvent getValue = do
  postAsync <- askPostAsync
  Build $ newEventWithTrigger $ \et -> onEvent element $
      getValue >>= liftIO . traverse_ (\v -> postAsync (et :=> v))


type Events t = EventSelector t (WrapArg EventResult EventName)

bindEvents :: (Renderer t, Dom.IsElement e) => e -> Builder t (Events t)
bindEvents dom = wrapDomEventsMaybe dom (defaultDomEventHandler dom)

wrapDomEventsMaybe :: (Renderer t, Dom.IsElement e) => e -> EventsHandler f e en -> Builder t (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe element handlers = do
  postAsync  <- askPostAsync
  Build $ newFanEventWithTrigger $ \(WrapArg en) et -> onEventName en element  $ do
      handlers en >>= liftIO . traverse_ (\v -> postAsync (et :=> v))



