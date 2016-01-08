{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables, RankNTypes, TupleSections, GADTs, TemplateHaskell, ConstraintKinds, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, LambdaCase, ImpredicativeTypes #-}

module Reflex.Html.Render  where

import qualified GHCJS.DOM as Dom
import qualified GHCJS.DOM.Node as Dom
import qualified GHCJS.DOM.Text as Dom
import qualified GHCJS.DOM.CharacterData as Dom
import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.EventM as Dom

import qualified GHCJS.DOM.Document as Doc

import Control.Monad.State.Strict
import Control.Monad.RSS.Strict

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Lens hiding (Traversal)

import Data.Dependent.Sum
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.Maybe
import Data.Proxy
import Data.Map (Map)
import qualified Data.Map as Map


import Data.Foldable
import Data.Functor.Misc

import Reflex
import Reflex.Dirty
import Reflex.Host.Class

import Data.IORef
import Data.Unsafe.Tag
import Reflex.Html.Event
import Reflex.Monad

import Control.Monad.Ref

import qualified Data.List.NonEmpty as NE
import Data.Semigroup.Applicative

type Impl = Spider
type Host = SpiderHost

runHost :: Host a -> IO a
runHost = runSpiderHost


data Env t = Env
  { envDoc    :: Dom.Document
  , envParent :: Dom.Node
  , envChan   :: Chan (DSum (EventTrigger t))
  }

type TriggerRef t a = IORef (Maybe (EventTrigger t a))
type Renderer t = (ReflexHost t, MonadIO (HostFrame t))

type RequestT t m = Dirty t (Traversal m)
type Request t = RequestT t (RenderT t (HostFrame t))

data TriggerOcc t = forall a. TriggerOcc (TriggerRef t a) a


data RenderEvents t = RenderEvents
  { rsFan      :: DMap Tag
  , rsTriggers :: [TriggerOcc t]
  }

newtype RenderT t m a = Render { unRender :: StateT (RenderEvents t) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadSample t, MonadHold t,
           MonadIO, MonadState (RenderEvents t), MonadReflexCreateTrigger t)

newtype BuilderT t m a = Build { unBuild :: RSST (Env t) () [RequestT t m]  m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadHold t, MonadSample t,
           MonadReader (Env t),  MonadState [RequestT t m], MonadReflexCreateTrigger t)

instance MonadTrans (BuilderT t) where
  lift = Build . lift

instance MonadHold t m => MonadHold t (RSST r w s m) where
  hold a0 = lift . hold a0

instance MonadSample t m => MonadSample t (RSST r w s m) where
  sample = lift . sample

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RSST r w s m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f



type DynMap t k a = (Behavior t (Map k a), Event t (Map k (Maybe a)))

type Builder t a = BuilderT t (RenderT t (HostFrame t)) a
type Render t a = RenderT t (HostFrame t) a

type EventsHandler f e en  = (forall en. EventName en -> Dom.EventM  e (EventType en)  (Maybe (f en)))

askDocument :: Renderer t => Builder t Dom.Document
askDocument = asks envDoc

askPostAsync :: Renderer t => Builder t (DSum (EventTrigger t) -> IO ())
askPostAsync = post <$> asks envChan
  where post chan tv = writeChan chan tv


postTriggerRef :: Renderer t => a -> TriggerRef t a -> Builder t ()
postTriggerRef a ref = do
  postAsync <- askPostAsync
  liftIO $ readIORef ref >>= traverse_ (\t -> postAsync (t :=> a))


readTriggerRef :: (MonadIO m) => proxy t -> TriggerOcc t -> m (Maybe (DSum (EventTrigger t)))
readTriggerRef _ (TriggerOcc tr a) = liftIO $ fmap (:=> a) <$> readIORef tr


renderTriggers :: forall t m. MonadIO m => TriggerRef t (DMap Tag) -> RenderEvents t -> m [DSum (EventTrigger t)]
renderTriggers tr events = do
  occs    <- traverse readTrigger (rsTriggers events)
  fanOcc  <- readTrigger  (TriggerOcc tr (rsFan events))
  return $ catMaybes (fanOcc:occs)
    where readTrigger = readTriggerRef (Proxy :: Proxy t)



return_ :: Renderer t => Tag a -> a -> Render t ()
return_ tag a = modify insertFan where
  insertFan re = re { rsFan = DMap.insert tag a (rsFan re) }


trigger :: Renderer t => TriggerRef t a -> a -> Render t ()
trigger tr a = modify insertFan where
  insertFan re = re { rsTriggers = (TriggerOcc tr a) : rsTriggers re }

newTriggerRef :: (MonadIO m, MonadReflexCreateTrigger t m) => m (Event t a, IORef (Maybe (EventTrigger t a)))
newTriggerRef = do
  rt <- liftIO $ newIORef Nothing
  e <- newEventWithTrigger $ \t -> do
    liftIO $ writeIORef rt $ Just t
    return $ liftIO $ writeIORef rt Nothing
  return (e, rt)
{-# INLINE newTriggerRef #-}

newEvent :: Renderer t => Render t (Event t a, a -> Render t ())
newEvent = do
  (e, tr) <- Render newTriggerRef
  return (e, trigger tr)


render :: Renderer t => Event t a -> (a -> Render t ()) -> Builder t ()
render = renderWith dirty

foldRender :: Renderer t => (a -> a -> a) -> Event t a -> (a -> Render t ()) -> Builder t ()
foldRender f = renderWith (foldDirty f)

renderWith :: Renderer t => (Event t a -> Event t () -> Builder t (Dirty t a))
           -> Event t a -> (a -> Render t ()) -> Builder t ()
renderWith f e toRender = do
  (reset, fire) <- lift newEvent
  let run a = Traversal $ toRender a >> fire ()
  r <- f e reset
  modify (fmap run r :)


resetEvent :: Renderer t => Builder t (Event t (), Traversal (RenderT t (HostFrame t)))
resetEvent = do
  (reset, fire) <- lift newEvent
  return (reset, Traversal (fire ()))

mergeRequests :: Renderer t => [Request t] -> Builder t (Request t)
mergeRequests reqs = do
  (reset, triggerReset) <- resetEvent
  req <- mergeDirty reqs reset
  return (mappend triggerReset <$> req)


holdRequests :: Renderer t => [Request t] -> Builder t (Behavior t (Maybe (Render t ())))
holdRequests reqs = do
  (reset, triggerReset) <- resetEvent
  req <- holdDirty reqs reset
  return (fmap (getTraversal . mappend triggerReset) <$> req)

fireTriggerRef :: (MonadIO m, MonadReflexHost t m) => TriggerRef t a -> a -> m ()
fireTriggerRef tr a = liftIO (readIORef tr) >>= traverse_ (\t -> fireEvents [t :=> a])


runBuilder ::  Renderer t =>  Env t -> Builder t () -> Render t (Behavior t (Maybe (Render t ())))
runBuilder env build = fst <$> evalRSST (unBuild $ build >> holdRender)  env []
    where holdRender = get >>= holdRequests

runRender ::  TriggerRef Impl (DMap Tag) -> Render Impl a -> Host a
runRender tr (Render render) = do
  (a, events) <- runHostFrame $ runStateT render (RenderEvents mempty mempty)
  fireEvents =<< renderTriggers tr events
  return a

sampleRender :: TriggerRef Impl (DMap Tag) -> Behavior Impl (Maybe (Render Impl ())) -> IO ()
sampleRender tr b = runHost $ sample b >>= traverse_ (runRender tr)


runAsync :: IO a -> (a -> IO ()) -> IO ThreadId
runAsync input action = forkIO $ forever $ do
  input >>= Dom.postGUIAsync . action


buildBody :: TriggerRef Impl (DMap Tag) -> Builder Impl () -> IO ()
buildBody tr build = Dom.runWebGUI $ \webView -> do
  Dom.enableInspector webView
  Just doc <- Dom.webViewGetDomDocument webView
  env <- Env doc
    <$> (Dom.toNode . fromJust <$> Doc.getBody doc)
    <*> newChan

  render <- runHost $ runRender tr $ runBuilder env build
  runAsync (threadDelay (1000000 `div` frameRate))
           (const $ sampleRender tr render)

  runAsync (readChan (envChan env))
           (runHost . fireEvents . pure)
  return ()

    where
      frameRate = 30

withParent :: Renderer t =>  (Dom.Node -> Dom.Document -> Builder t a) -> Builder t a
withParent f = do
  env <- ask
  f (envParent env) (envDoc env)


buildDyn :: Renderer t => Dynamic t (Builder t ()) -> Builder t ()
buildDyn d = return ()


buildElement_ :: Renderer t => String -> String -> DynMap t String String -> Builder t Dom.Element
buildElement_ ns tag (currentA, updatedA) = withParent $ \parent doc -> do
  Just e <- liftIO $ Doc.createElementNS doc (Just ns) (Just tag)
  sample currentA >>= imapM_ (Dom.setAttribute e)
  foldRender (flip Map.union) updatedA $
    liftIO . imapM_ (addRemove e)

  liftIO $ Dom.appendChild parent $ Just e
  return e

addRemove :: Dom.Element -> String -> Maybe String -> IO ()
addRemove e name (Just v) = Dom.setAttribute e name v
addRemove e name Nothing  = Dom.removeAttribute e name

buildText :: Renderer t => String -> Builder t Dom.Text
buildText str = withParent $ \parent doc -> liftIO $ do
  Just dom <- Doc.createTextNode doc str
  void $ Dom.appendChild parent $ Just dom
  return dom

updateText :: MonadIO m => Dom.Text -> String -> m ()
updateText text =  liftIO . void . Dom.setData text . Just


buildElement :: Renderer t => String -> String -> DynMap t String String -> Builder t a -> Builder t (Dom.Element, a)
buildElement ns tag attrs child = do
  dom <- buildElement_ ns tag attrs
  a <- local (reParent (Dom.toNode dom)) child
  return (dom, a)

  where
    reParent dom e = e { envParent = dom }

-- Event Trigger creation from input events
wrapDomEvent :: (Renderer t, Dom.IsElement e, Dom.EventClass event) => e ->  Dom.EventName e event -> Dom.EventM e event a -> Builder t (Event t a)
wrapDomEvent element eventName getValue = wrapDomEventMaybe element eventName $ Just <$> getValue

wrapDomEventMaybe :: (Renderer t, Dom.IsElement e, Dom.EventClass event) => e -> Dom.EventName e event -> Dom.EventM e event (Maybe a) -> Builder t (Event t a)
wrapDomEventMaybe element eventName getValue = do
  postAsync <- askPostAsync
  Build $ newEventWithTrigger $ \et -> Dom.on element eventName $
      getValue >>= liftIO . traverse_ (\v -> postAsync (et :=> v))


type Events t = EventSelector t (WrapArg EventResult EventName)

bindEvents :: (Renderer t, Dom.IsElement e) => e -> Builder t (Events t)
bindEvents dom = wrapDomEventsMaybe dom (defaultDomEventHandler dom)

wrapDomEventsMaybe :: (Renderer t, Dom.IsElement e) => e -> EventsHandler f e en -> Builder t (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe element handlers = do
  postAsync  <- askPostAsync
  Build $ newFanEventWithTrigger $ \(WrapArg en) et -> onEventName en element  $ do
      handlers en >>= liftIO . traverse_ (\v -> postAsync (et :=> v))



