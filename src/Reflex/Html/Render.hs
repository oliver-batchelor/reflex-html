{-# LANGUAGE UndecidableInstances, ImpredicativeTypes #-}
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
import Control.Monad.Trans.RSS.Strict
import Control.Monad.RSS.Strict

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Lens hiding (Traversal)

import Data.Dependent.Sum
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.Proxy
import qualified Data.Map as Map

import Reflex.Host.Class

import Data.IORef
import Data.Functor.Misc
import Data.Unsafe.Tag


import Reflex.Html.Event
import Reflex.Html.Prelude

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
data TriggerOcc t = forall a. TriggerOcc (TriggerRef t a) a

type Renderer t = (ReflexHost t, MonadIO (HostFrame t))
type Request t = Behavior t (Traversal (Render t))

data RenderEvents t = RenderEvents
  { rsFan      :: DMap Tag
  , rsTriggers :: [TriggerOcc t]
  }

newtype Render t a = Render { unRender :: StateT (RenderEvents t) (HostFrame t) a }

deriving instance Renderer t => Functor (Render t)
deriving instance Renderer t => Applicative (Render t)
deriving instance Renderer t => Monad (Render t)
deriving instance Renderer t => MonadFix (Render t)
deriving instance Renderer t => MonadIO (Render t)
deriving instance Renderer t => MonadSample t (Render t)
deriving instance Renderer t => MonadHold t (Render t)
deriving instance Renderer t => MonadReflexCreateTrigger t (Render t)

newtype Builder t a = Build { unBuild :: RSST (Env t) () [Request t]  (Render t) a }

deriving instance Renderer t => Functor (Builder t)
deriving instance Renderer t => Applicative (Builder t)
deriving instance Renderer t => Monad (Builder t)
deriving instance Renderer t => MonadFix (Builder t)
deriving instance Renderer t => MonadIO (Builder t)
deriving instance Renderer t => MonadSample t (Builder t)
deriving instance Renderer t => MonadHold t (Builder t)
deriving instance Renderer t => MonadReflexCreateTrigger t (Builder t)


instance (Renderer t) => Switching t (Builder t ()) where
  switching initial e = buildDyn <$> holdDyn initial e

instance (Renderer t) => SwitchConcat t (Builder t ()) where
  switchConcat initial e = buildMap e <$> holdMap (UpdatedMap initial e)


instance MonadHold t m => MonadHold t (RSST r w s m) where
  hold a0 = lift . hold a0

instance MonadSample t m => MonadSample t (RSST r w s m) where
  sample = lift . sample

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RSST r w s m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

type DynMap t k a = (Behavior t (Map k a), Event t (Map k (Maybe a)))

type Events t = EventSelector t (WrapArg EventResult EventName)
type EventsHandler f e en  = (forall en. EventName en -> Dom.EventM  e (EventType en)  (Maybe (f en)))

askDocument :: Renderer t => Builder t Dom.Document
askDocument = Build $ asks envDoc

askPostAsync :: Renderer t => Builder t (DSum (EventTrigger t) -> IO ())
askPostAsync = post <$> Build (asks envChan)
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
return_ tag a = Render $ modify insertFan where
  insertFan re = re { rsFan = DMap.insert tag a (rsFan re) }


trigger :: Renderer t => TriggerRef t a -> a -> Render t ()
trigger tr a = Render $ modify insertFan where
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

holdReset :: (MonadReflex t m) => Event t a -> Event t b -> m (Behavior t (Maybe a))
holdReset e reset = hold Nothing $ leftmost [Just <$> e, Nothing <$ reset]


foldReset :: (MonadReflex t m) => (a -> a -> a) -> Event t a -> Event t b -> m (Behavior t (Maybe a))
foldReset f e reset = current <$> foldDyn accum Nothing inp where
  inp = leftmost [Just <$> e, Nothing <$ reset]

  accum Nothing   _ = Nothing
  accum (Just a)  Nothing = Just a
  accum (Just a) (Just b) = Just (f a b)


render :: Renderer t => Event t a -> (a -> Render t b) -> Builder t (Event t b)
render = renderWith holdReset


foldRender :: Renderer t => (a -> a -> a) -> Event t a -> (a -> Render t b) -> Builder t (Event t b)
foldRender f = renderWith (foldReset f)

request :: Renderer t => Request t -> Builder t ()
request r = Build $ modify (r :)

renderAlways :: Renderer t => Behavior t a -> (a -> Render t ()) -> Builder t ()
renderAlways b f = request $ Traversal . f <$> b

renderWith :: Renderer t => (Event t a -> Event t b -> Builder t (Behavior t (Maybe a)))
           -> Event t a -> (a -> Render t b) -> Builder t (Event t b)
renderWith accumReset e toRender = do
  (result, fire) <- Build $ lift newEvent
  r <- accumReset e result
  let run = maybe mempty (Traversal . (fire <=< toRender))
  request (run <$> r)
  return result

fireTriggerRef :: (MonadIO m, MonadReflexHost t m) => TriggerRef t a -> a -> m ()
fireTriggerRef tr a = liftIO (readIORef tr) >>= traverse_ (\t -> fireEvents [t :=> a])

runBuilder ::  Renderer t =>  Env t -> Builder t a -> Render t (a, Request t)
runBuilder env (Build build) = do
  (a, reqs, _) <- runRSST build env []
  return (a, mconcat reqs)

runRender ::  TriggerRef Impl (DMap Tag) -> Render Impl a -> Host a
runRender tr (Render render) = do
  (a, events) <- runHostFrame $ runStateT render (RenderEvents mempty mempty)
  fireEvents =<< renderTriggers tr events
  return a

sampleRender :: TriggerRef Impl (DMap Tag) -> Request Impl -> IO ()
sampleRender tr b = runHost $ sample b >>= runRender tr . getTraversal


runAsync :: IO a -> (a -> IO ()) -> IO ThreadId
runAsync input action = forkIO $ forever $ do
  input >>= Dom.postGUIAsync . action


buildBody :: TriggerRef Impl (DMap Tag) -> Builder Impl () -> IO ()
buildBody tr build = Dom.runWebGUI $ \webView -> void $ do
  Dom.enableInspector webView
  Just doc <- Dom.webViewGetDomDocument webView
  env <- Env doc
    <$> (Dom.toNode . fromJust <$> Doc.getBody doc)
    <*> newChan

  render <- runHost $ runRender tr (snd <$> runBuilder env build)
  runAsync (threadDelay (1000000 `div` frameRate))
           (const $ sampleRender tr render)

  runAsync (readChan (envChan env))
           (runHost . fireEvents . pure)

    where
      frameRate = 30

withParent :: Renderer t =>  (Dom.Node -> Dom.Document -> Builder t a) -> Builder t a
withParent f = do
  env <- Build ask
  f (envParent env) (envDoc env)


type Range = (Dom.Node, Dom.Node)

deleteExclusive :: Range -> IO ()
deleteExclusive (start, end) = traverse_ removeFrom =<< Dom.getParentNode end  where
  removeFrom parent = do
    node <- Dom.getPreviousSibling end
    when (Just start /= node) $ do
      Dom.removeChild parent node >> removeFrom parent

makeRange :: Renderer t => Builder t () -> Builder t Range
makeRange b = (,) <$> marker <*> (b >> marker)


buildDyn :: Renderer t => Dynamic t (Builder t ()) -> Builder t ()
buildDyn d = do
  env <- Build ask
  (range, req) <- makeRange <$> sample (current d) >>=
    Build . lift . runBuilder env

  updatedReq <- render (updated d) $ replaceRange range env
  request =<< switching req updatedReq


splitF :: (Functor f, Functor g) => f (g (a, b)) -> (f (g a), f (g b))
splitF f = (fmap fst <$> f, fmap snd <$> f)

buildMap :: (Renderer t, Ord k) => Event t (Map k (Maybe (Builder t ()))) -> Behavior t (Map k (Builder t ())) -> Builder t ()
buildMap e b = do
  env <- Build ask
  let run = Build . lift . runBuilder env . makeRange
  (ranges0, reqs0) <- split <$> (traverse run =<< sample b)
  end <- marker

  rec
    ranges  <- hold ranges0 rangeChanges
    (updatedReqs, rangeChanges) <- fmap split $ foldRender Map.union e $ \changes -> do
      (frags, reqs) <- splitF <$> traverse (traverse (inFragment env)) changes
      (reqs, ) <$> (liftIO . updateList end frags =<< sample ranges)

  request =<< switchConcat reqs0 updatedReqs

updateList :: Dom.Node -> Map k (Maybe Dom.DocumentFragment) -> Map k Range -> IO (Map k Range)
updateList end frags ranges = ifoldlM update ranges frags where
  update = undefined


replaceRange :: Renderer t => (Dom.Node, Dom.Node) -> Env t -> Builder t () -> Render t (Request t)
replaceRange (start, end) env b = do
  (frag, req) <- inFragment env b
  liftIO $ do
    deleteExclusive (start, end)
    Dom.insertBefore (envParent env) (Just frag) (Just end)
  return req


inFragment :: Renderer t => Env t -> Builder t () -> Render t (Dom.DocumentFragment, Request t)
inFragment env b = do
  (Just frag) <- liftIO $ Doc.createDocumentFragment (envDoc env)
  (_, req) <- runBuilder (env {envParent = Dom.toNode frag}) b
  return (frag, req)



buildElement_ :: Renderer t => DomString -> DomString -> DynMap t DomString DomString -> Builder t Dom.Element
buildElement_ ns tag (currentA, updatedA) = withParent $ \parent doc -> do
  Just e <- liftIO $ Doc.createElementNS doc (Just ns) (Just tag)
  sample currentA >>= imapM_ (Dom.setAttribute e)
  foldRender (flip Map.union) updatedA $
    liftIO . imapM_ (addRemove e)

  liftIO $ Dom.appendChild parent $ Just e
  return e

addRemove :: Dom.Element -> DomString -> Maybe DomString -> IO ()
addRemove e name (Just v) = Dom.setAttribute e name v
addRemove e name Nothing  = Dom.removeAttribute e name

marker :: Renderer t => Builder t Dom.Node
marker = Dom.toNode <$> buildText ""

buildText :: Renderer t => DomString -> Builder t Dom.Text
buildText str = withParent $ \parent doc -> liftIO $ do
  Just dom <- Doc.createTextNode doc str
  void $ Dom.appendChild parent $ Just dom
  return dom

updateText :: MonadIO m => Dom.Text -> DomString -> m ()
updateText text =  liftIO . void . Dom.setData text . Just


buildElement :: Renderer t => DomString -> DomString -> DynMap t DomString DomString -> Builder t a -> Builder t (Dom.Element, a)
buildElement ns tag attrs (Build child) = do
  dom <- buildElement_ ns tag attrs
  a <- Build $ local (reParent (Dom.toNode dom)) child
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



bindEvents :: (Renderer t, Dom.IsElement e) => e -> Builder t (Events t)
bindEvents e = wrapDomEventsMaybe e (defaultDomEventHandler e)


wrapDomEventsMaybe :: (Renderer t, Dom.IsElement e) => e -> EventsHandler f e en -> Builder t (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe element handlers = do
  postAsync  <- askPostAsync
  Build $ newFanEventWithTrigger $ \(WrapArg en) et -> onEventName en element  $ do
      handlers en >>= liftIO . traverse_ (\v -> postAsync (et :=> v))



