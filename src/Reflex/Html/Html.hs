
{-# LANGUAGE ConstraintKinds, RankNTypes, GADTs, TemplateHaskell, ConstraintKinds, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Reflex.Html.Html  where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.Trans.RSS.Strict

import Control.Lens hiding (Traversal, runTraversal)

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum

import qualified Data.DList as DL
import Data.DList (DList)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Default
import Data.Functor
import Data.Foldable
import Data.Semigroup
import Data.Semigroup.Applicative

import Reflex
import Reflex.Dirty
import Reflex.Host.Class

import Reflex.Monad
import Reflex.Monad.ReflexM

import Data.GADT.Compare.TH
import Data.Functor.Misc

import qualified Control.Concurrent.Supply as S

import Reflex.Html.Event
import Reflex.Html.Render
import Data.Unsafe.Tag


type BuilderM t = Traversal (BuilderT t (RenderT t (HostFrame t)))

newtype HtmlT t m a = Html
  { unHtml :: RSST
      (EventSelector t Tag)
      (BuilderM t)
      S.Supply
        m a
  }
    deriving (Functor, Applicative, Monad, MonadFix, MonadSample t, MonadHold t,
             MonadTrans, MonadReader (EventSelector t Tag))


type Html t a = HtmlT t (ReflexM t) a


class HasDomEvent t a where
  domEvent :: EventName en -> a -> Event t (EventResultType en)


class Reflex (T r) => HasReflex r where
  type T r :: *

newtype Element t = Element { elementEvents :: Events t }

instance Reflex t => HasReflex (Element t) where
  type T (Element t) = t

instance Reflex t => HasDomEvent t (Element t) where
  domEvent en e = unEventResult <$> select (elementEvents e) (WrapArg en)


htmlNs = "http://www.w3.org/1999/xhtml"

freshTag :: Html t (Tag a)
freshTag = Html $ Tag <$> state S.freshId

eventTag :: Renderer t => Html t (Event t a, Tag a)
eventTag = do
  tag <- freshTag
  e <- asks (flip select tag)
  return (e, tag)

build_ :: Renderer t => Builder t a -> Html t ()
build_  = Html .  tell . Traversal . void


build' :: Renderer t =>  Builder t a -> Html t (Event t a)
build' r = do
  tag <- freshTag
  Html . tell $ Traversal (r >>= lift . return_ tag)
  asks (flip select tag)

build :: (Default a, Switching t a, Renderer t) => Builder t a -> Html t a
build = build' >=> delayed

instance Reflex t => Default (Event t a) where
  def = never

instance (Reflex t, Monoid a) => Default (Behavior t a) where
  def = mempty

delayed :: (MonadReflex t m, Default a, Switching t a) => Event t a -> m a
delayed = switching def

holdBuild :: Renderer t => Builder t a -> Html t (Behavior t (Maybe a))
holdBuild r = hold Nothing =<< fmap Just <$> build' r


clicked :: Reflex t => Element t -> Event t ()
clicked  = domEvent Click

instance Reflex t => Default (EventSelector t k) where
  def = EventSelector (const never)

instance Reflex t => Switching t (EventSelector t k) where
  switching initial e = do
    b <- hold initial e
    return $ EventSelector $ \k -> switch (flip select k <$> b)


collectBuilder :: Renderer t => Html t a -> Html t (a, Builder t ())
collectBuilder (Html m) = Html $ do
  (a, Traversal r) <- collectRSST m
  return (a, r)


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






