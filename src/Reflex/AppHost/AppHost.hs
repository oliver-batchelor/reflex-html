{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module {-Reflex.QML.Internal.-}AppHost where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Dependent.Sum
import Data.IORef
import Data.Maybe
import Data.Semigroup.Applicative
import Data.Tuple
import Prelude
import Reflex.Class hiding (constant)
import Reflex.Dynamic
import Reflex.Host.Class

import qualified Data.DList as DL
import qualified Data.Foldable as F
import qualified Data.Traversable as T

--------------------------------------------------------------------------------

-- | This is the environment in which the app host monad runs.
data AppEnv t = AppEnv
  { -- | This is the channel to which external events should push their triggers.
    --
    -- Because this is a channel, there is no guarrante that the event that was pushed
    -- is fired directly in the next frame, as there can already be other events waiting
    -- which will be fired first.
    envEventChan :: Chan [DSum (EventTrigger t)]
  }

-- | An action that is run after a frame. It may return event triggers to fire events.
--
type AppPerformAction t = HostFrame t (DL.DList (DSum (EventTrigger t)))

-- | Information required to set up the application.
data AppInfo t = AppInfo
  { -- | Events that are performed after each frame.
    --
    -- Each event in this list will be checked after a frame. If it is firing with some
    -- 'AppPerformAction', then this action will be executed.
    --
    -- The event triggers returned by each 'AppPerformAction' are collected in a list.
    -- If the list is non-empty, then a new frame will be created where all the collected
    -- triggers are fired at the same time. This process continues until no triggers are
    -- produced.
    --
    -- The returned triggers are fired immediately, even if the 'envEventChan' currently
    -- contains other triggers waiting to be fired. The events in the 'envEventChan' are
    -- only processed after all triggers have been fired and no new triggers were
    -- produced.
    --
    -- A common place where you need this is when you want to fire some event in response
    -- to another event, but you need to perform a monadic action to compute the value of
    -- the response. Using this field, you can perform the monadic action and then return
    -- a trigger to fire the event. This guarrantes that the event is fired immediately
    -- after the frame has finished, even if other, external events are waiting.
    eventsToPerform :: DL.DList (Event t (AppPerformAction t))

    -- | Events that, when fired, quit the application.
  , eventsToQuit :: DL.DList (Event t ())

    -- | Delayed event triggers that will be fired immediately after the initial
    -- application setup has completed, before any external events are processed.
  , triggersToFire :: Ap (HostFrame t) (DL.DList (DSum (EventTrigger t)))
  }

instance Applicative (HostFrame t) => Monoid (AppInfo t) where
  mempty = AppInfo mempty mempty mempty
  mappend (AppInfo a b c) (AppInfo a' b' c') =
    AppInfo (mappend a a') (mappend b b') (mappend c c')

-- | Produce an 'AppInfo' which only contains 'eventsToPerform'. This is useful in a
-- monoid chain, like @infoToPerform toPerform <> infoToQuit toQuit@.
infoPerform :: Applicative (HostFrame t)
            => DL.DList (Event t (AppPerformAction t)) -> AppInfo t
infoPerform x = mempty { eventsToPerform = x }

-- | Produce an 'AppInfo' which only contains 'eventsToQuit'.
infoQuit :: Applicative (HostFrame t) => DL.DList (Event t ()) -> AppInfo t
infoQuit x = mempty { eventsToQuit = x }

-- | Produce an 'AppInfo' which only contains 'triggersToFire'.
infoFire :: Applicative (HostFrame t)
           => HostFrame t (DL.DList (DSum (EventTrigger t))) -> AppInfo t
infoFire x = mempty { triggersToFire = Ap x }

appInfoEvents :: (Reflex t, Applicative (HostFrame t))
              => AppInfo t -> (Event t (AppPerformAction t), Event t ())
appInfoEvents AppInfo{..} =
  ( mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  , leftmost $ DL.toList eventsToQuit
  )

switchAppInfo :: (ReflexHost t, MonadHold t m, Reflex t)
              => AppInfo t -> Event t (AppInfo t) -> m (AppInfo t)
switchAppInfo initialInfo updatedInfo = do
  toPerform <- switch <$> hold initialToPerform updatedToPerform
  toQuit    <- switch <$> hold initialToQuit updatedToQuit
  pure $ AppInfo
    { eventsToPerform = pure toPerform <> pure (getApp . triggersToFire <$> updatedInfo)
    , eventsToQuit = pure toQuit
    , triggersToFire = triggersToFire initialInfo
    }
 where
  (updatedToPerform, updatedToQuit) = splitE $ fmap appInfoEvents updatedInfo
  (initialToPerform, initialToQuit) = appInfoEvents initialInfo

--------------------------------------------------------------------------------
newtype AppHost t a = AppHost
  { unAppHost :: ReaderT (AppEnv t) (WriterT (Ap (HostFrame t) (AppInfo t)) (HostFrame t)) a
  }
deriving instance ReflexHost t => Functor (AppHost t)
deriving instance ReflexHost t => Applicative (AppHost t)
deriving instance ReflexHost t => Monad (AppHost t)
deriving instance ReflexHost t => MonadHold t (AppHost t)
deriving instance ReflexHost t => MonadSample t (AppHost t)
deriving instance ReflexHost t => MonadReflexCreateTrigger t (AppHost t)
deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (AppHost t)
deriving instance ReflexHost t => MonadFix (AppHost t)

execAppHostFrame :: ReflexHost t => AppEnv t -> AppHost t a -> HostFrame t (AppInfo t)
execAppHostFrame env = fmap fst . runAppHostFrame env

runAppHostFrame :: ReflexHost t => AppEnv t -> AppHost t a -> HostFrame t (AppInfo t, a)
runAppHostFrame env app = do
  (a, Ap minfo) <- runWriterT . flip runReaderT env . unAppHost $ app
  (, a) <$> minfo

hostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m) => AppHost t () -> m ()
hostApp app = do
  env <- AppEnv <$> liftIO newChan
  AppInfo{..} <- runHostFrame $ execAppHostFrame env app
  nextActionEvent <- subscribeEvent $ mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  quitEvent <- subscribeEvent $ mergeWith mappend $ DL.toList eventsToQuit

  let
    go [] = return ()
    go triggers = do
      (nextAction, continue) <- lift $ fireEventsAndRead triggers $
        (,) <$> eventValue nextActionEvent <*> fmap isNothing (readEvent quitEvent)
      guard continue
      maybe (return mempty) (lift . runHostFrame) nextAction >>= go . DL.toList

    eventValue :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
    eventValue = readEvent >=> T.sequenceA

  void . runMaybeT $ do
    go =<< lift (runHostFrame (DL.toList <$> getApp triggersToFire))
    forever $ do
      nextInput <- liftIO . readChan $ envEventChan env
      go nextInput
  return ()

--------------------------------------------------------------------------------
class (ReflexHost t, MonadSample t m, MonadHold t m, MonadReflexCreateTrigger t m,
       MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadFix (HostFrame t))
      => MonadAppHost t m | m -> t where
  getAsyncFire :: m ([DSum (EventTrigger t)] -> IO ())
  getRunAppHost :: m (m a -> HostFrame t (HostFrame t (AppInfo t), a))
  performPostBuild_ :: HostFrame t (AppInfo t) -> m ()
  liftHostFrame :: HostFrame t a -> m a

instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppHost t) where
  getAsyncFire = AppHost $ fmap liftIO . writeChan . envEventChan <$> ask
  getRunAppHost = AppHost $ do
    env <- ask
    let rearrange (a, Ap m) = (m, a)
    pure $ fmap rearrange . runWriterT . flip runReaderT env . unAppHost
  performPostBuild_ mevent = AppHost . tell $ Ap mevent
  liftHostFrame = AppHost . lift . lift

--------------------------------------------------------------------------------
newEventWithConstructor
  :: MonadAppHost t m => m (Event t a, a -> IO (Maybe (DSum (EventTrigger t))))
newEventWithConstructor = do
  ref <- liftIO $ newIORef Nothing
  event <- newEventWithTrigger (\h -> writeIORef ref Nothing <$ writeIORef ref (Just h))
  return (event, \a -> fmap (:=> a) <$> liftIO (readIORef ref))

newExternalEvent :: MonadAppHost t m => m (Event t a, a -> IO Bool)
newExternalEvent = do
  asyncFire <- getAsyncFire
  (event, construct) <- newEventWithConstructor
  return (event, fmap isJust . traverse (asyncFire . pure) <=< construct)

performEventAndTrigger_ :: MonadAppHost t m => Event t (AppPerformAction t) -> m ()
performEventAndTrigger_ = performPostBuild_ . pure . infoPerform . pure

performEvent_ :: MonadAppHost t m => Event t (HostFrame t ()) -> m ()
performEvent_ = performEventAndTrigger_ . fmap (mempty <$)

performEvent :: MonadAppHost t m => Event t (HostFrame t a) -> m (Event t a)
performEvent event = do
  (result, construct) <- newEventWithConstructor
  performEventAndTrigger_ $ (fmap (F.foldMap pure) . liftIO . construct =<<) <$> event
  return result

runAppHost :: MonadAppHost t m => m a -> m (HostFrame t (AppInfo t), a)
runAppHost action = liftHostFrame . ($ action) =<< getRunAppHost

switchAppHost :: MonadAppHost t m => HostFrame t (AppInfo t) -> Event t (m a) -> m (Event t a)
switchAppHost initial event = do
  run <- getRunAppHost
  let runWithPost = run >=> \(post, a) -> (,a) <$> post
  (infoEvent, valueEvent) <- fmap splitE . performEvent $ runWithPost <$> event
  performPostBuild_ $ flip switchAppInfo infoEvent =<< initial
  return valueEvent

performAppHost :: MonadAppHost t m => Event t (m a) -> m (Event t a)
performAppHost = switchAppHost (pure mempty)

dynAppHost :: MonadAppHost t m => Dynamic t (m a) -> m (Event t a)
dynAppHost dyn = do
  run <- getRunAppHost
  (initialEvent, initialConstruct) <- newEventWithConstructor
  updatedEvent <- flip switchAppHost (updated dyn) $ do
    (minfo, r) <- sample (current dyn) >>= run
    info <- minfo
    trigger <- liftIO $ initialConstruct r
    pure $ info <> F.foldMap (infoFire . pure . pure) trigger
  pure $ leftmost [updatedEvent, initialEvent]

holdAppHost :: MonadAppHost t m => m a -> Event t (m a) -> m (Dynamic t a)
holdAppHost mInit mChanged = do
  (postActions, aInit) <- runAppHost mInit
  aChanged <- switchAppHost postActions mChanged
  holdDyn aInit aChanged
