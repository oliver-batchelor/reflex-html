{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}

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

type HtmlM t a = RSST
      (EventSelector t Tag)
      (Build t)
      S.Supply
        (ReflexM t) a

newtype Html t a = Html { unHtml :: HtmlM t a }

deriving instance Renderer t => Functor (Html t)
deriving instance Renderer t => Applicative (Html t)
deriving instance Renderer t => Monad (Html t)
deriving instance Renderer t => MonadFix (Html t)
deriving instance Renderer t => MonadSample t (Html t)
deriving instance Renderer t => MonadHold t (Html t)

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


build_ ::  Renderer t => Builder t () -> Html t ()
build_  = Html .  tell . Traversal . void

build  ::  Renderer t => Builder t a -> Html t (Event t a)
build r = Html $ do
  tag <- freshTag
  tell . Traversal . return' $ tag
  asks (flip select tag)
      where return' tag = r >>= Build . lift . return_ tag

runChild  :: Renderer t => Html t a -> Html t (a, Builder t ())
runChild (Html m) = Html $ do
  (a, Traversal r) <- collectRSST m
  return (a, r)


liftHtml :: ReflexM t a -> Html t a
liftHtml = Html . lift

instance Renderer t => MonadSwitch t (Html t) where
  switchM (Updated initial e) = do
    s <- getSplit
    env <- Html ask
    rec
      (a, us, b) <- liftHtml (split3 <$>
        switchM (Updated (run env s initial) $ attachWith (run env) r e))
      r <- hold' us

    build_ =<< switching' (getTraversal <$> b)
    return a

    where
      run env s (Html m) = runRSST m env s

  switchMapM (UpdatedMap initial e) = do
    env <- Html ask
    (initial', s) <- runSupplyMap initial env <$> getSplit

    rec
      let (um, us) = split $ attachWith (runSupplyMap' env) r e
      a <- liftHtml (switchMapM (UpdatedMap initial' um))
      r <- hold s us

    build_ =<< switchConcat' (getTraversal . snd <$> a)
    return (fst <$> a)



getSplit :: Html t S.Supply
getSplit = Html $ state S.splitSupply

freshTag :: HtmlM t (Tag a)
freshTag =  Tag <$> state S.freshId

eventTag :: Reflex t => Html t (Event t a, Tag a)
eventTag = Html $ do
  tag <- freshTag
  e <- asks (flip select tag)
  return (e, tag)


switchBuild :: (Default a, Switching t a, Renderer t) => Builder t a -> Html t a
switchBuild = build >=> delayed

delayed :: (MonadReflex t m, Default a, Switching t a) => Event t a -> m a
delayed = switching def

holdBuild :: Renderer t => Builder t a -> Html t (Behavior t (Maybe a))
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

runSupplyMap :: (Ord k, Renderer t) =>  Map k (Html t a) ->
      EventSelector t Tag -> S.Supply -> (Map k (ReflexM t (a, Build t)), S.Supply)
runSupplyMap m env = runState (traverse (runSplit env) m)

runSupplyMap' :: (Ord k, Renderer t) => EventSelector t Tag ->  S.Supply
       ->  Map k (Maybe (Html t a))
       -> (Map k (Maybe (ReflexM t (a, Build t))), S.Supply)
runSupplyMap' env s m =  runState (traverse (traverse (runSplit env)) m) s

runSplit :: Renderer t => EventSelector t Tag -> Html t a -> State S.Supply (ReflexM t (a, Build t))
runSplit env (Html m) = evalRSST m env <$> state S.splitSupply

