{-# LANGUAGE DefaultSignatures #-}

module Reflex.Html.Html  where

import Control.Monad.Trans.RSS.Strict
import Control.Monad.State
import Control.Lens hiding (Traversal, runTraversal)

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum

import qualified Data.Map as Map
import Data.Semigroup.Applicative

import Reflex.Host.Class
import Reflex.Monad.ReflexM

import qualified Control.Concurrent.Supply as S

import Reflex.Html.Event
import Reflex.Html.Render
import Data.Unsafe.Tag

import Reflex.Html.Prelude

type Build t = Traversal (Builder t)

newtype HtmlT t m a = Html
  { unHtml :: RSST
      (EventSelector t Tag)
      (Build t)
      S.Supply
        m a
  }
    deriving (Functor, Applicative, Monad, MonadFix, MonadSample t, MonadHold t,
             MonadTrans, MonadReader (EventSelector t Tag))


type Html t a = HtmlT t (ReflexM t) a

class (Renderer t, MonadSwitch t m) => MonadWidget t m where
  build_ :: Builder t ()  -> m ()
  build  :: Builder t a   -> m (Event t a)
  runChild  :: m a        -> m (a, Builder t ())

  -- Default instances for MonadTrans transformers
  default build_ :: (MonadTrans u, MonadSwitch t (u m)) => Builder t () -> u m ()
  build_ = lift . build_

  default build :: (MonadTrans u, MonadSwitch t (u m)) => Builder t a -> u m (Event t a)
  build = lift . build


instance Reflex t => Default (Event t a) where
  def = never

instance (Reflex t, Monoid a) => Default (Behavior t a) where
  def = mempty


instance Reflex t => Default (EventSelector t k) where
  def = EventSelector (const never)

instance Reflex t => Switching t (EventSelector t k) where
  switching initial e = do
    b <- hold initial e
    return $ EventSelector $ \k -> switch (flip select k <$> b)


instance (Renderer t, MonadSwitch t m) => MonadWidget t (HtmlT t m) where
  build_  = Html .  tell . Traversal . void

  build r = do
    tag <- freshTag
    Html . tell . Traversal . return' $ tag
    asks (flip select tag)
      where return' tag = r >>= Build . lift . return_ tag

  runChild (Html m) = Html $ do
    (a, Traversal r) <- collectRSST m
    return (a, r)



instance (Renderer t, MonadSwitch t m) => MonadSwitch t (HtmlT t m) where
  switchM (Updated initial e) = do
    s <- getSplit
    env <- ask
    rec
      (a, us, b) <- lift (split3 <$>
        switchM (Updated (run env s initial) $ attachWith (run env) r e))
      r <- hold' us

    build_ =<< switching' (getTraversal <$> b)
    return a

    where
      run env s (Html m) = runRSST m env s

  switchMapM (UpdatedMap initial e) = do
    env <- ask
    (initial', s) <- runSupplyMap initial env <$> getSplit

    rec
      let (um, us) = split $ attachWith (runSupplyMap' env) r e
      a <- lift (switchMapM (UpdatedMap initial' um))
      r <- hold s us

    build_ =<< switchConcat' (getTraversal . snd <$> a)
    return (fst <$> a)



getSplit :: Monad m => HtmlT t m S.Supply
getSplit = Html $ state S.splitSupply

freshTag :: Monad m => HtmlT t m (Tag a)
freshTag = Html $ Tag <$> state S.freshId

eventTag :: (Reflex t, Monad m) => HtmlT t m (Event t a, Tag a)
eventTag = do
  tag <- freshTag
  e <- asks (flip select tag)
  return (e, tag)


switchBuild :: (Default a, Switching t a, MonadWidget t m) => Builder t a -> m a
switchBuild = build >=> delayed

delayed :: (MonadReflex t m, Default a, Switching t a) => Event t a -> m a
delayed = switching def

holdBuild :: MonadWidget t m => Builder t a -> m (Behavior t (Maybe a))
holdBuild r = hold Nothing =<< fmap Just <$> build r



runReflexFrame :: ReflexM Spider a -> SpiderHost a
runReflexFrame m = runHostFrame (runReflexM m)

runHtmlFrame :: Event Spider (DMap Tag) -> Html Spider () -> IO (Builder Spider ())
runHtmlFrame e (Html m) = runSpiderHost $ do
  s <- liftIO S.newSupply
  (_, build) <- runReflexFrame $ evalRSST m (fan e) s
  return (getTraversal build)

htmlBody :: Html Spider () -> IO ()
htmlBody html = do
  (e, ref) <- runSpiderHost newEventWithTriggerRef
  builder <- runHtmlFrame e html
  buildBody ref builder



-- Helper functions to support the MonadSwitch instance
split3 :: Functor f => f (a, b, c) -> (f a, f b, f c)
split3 f = (view _1 <$> f, view _2 <$> f, view _3 <$> f)

runSupplyMap :: (Ord k, Monad m, Renderer t) =>  Map k (HtmlT t m a) ->
      EventSelector t Tag -> S.Supply -> (Map k (m (a, Build t)), S.Supply)
runSupplyMap m env = runState (traverse (runSplit env) m)

runSupplyMap' :: (Ord k, Monad m, Renderer t) => EventSelector t Tag ->  S.Supply
       ->  Map k (Maybe (HtmlT t m a))
       -> (Map k (Maybe (m (a, Build t))), S.Supply)
runSupplyMap' env s m =  runState (traverse (traverse (runSplit env)) m) s

runSplit :: (Monad m, Renderer t) => EventSelector t Tag -> HtmlT t m a -> State S.Supply (m (a, Build t))
runSplit env (Html m) = evalRSST m env <$> state S.splitSupply

