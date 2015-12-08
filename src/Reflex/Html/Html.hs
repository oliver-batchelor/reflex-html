
{-# LANGUAGE RankNTypes, GADTs, TemplateHaskell, ConstraintKinds, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Html  where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.RSS.Strict

import Control.Lens hiding (Traversal)

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Semigroup

import Reflex
import Reflex.Host.Class

import Reflex.Monad
import Reflex.Monad.ReaderWriter
import Reflex.Monad.ReflexM

import Data.GADT.Compare.TH
import Data.Functor.Misc

import Event
import Render
import Tag

newtype Html t a = Html { unDom :: RSST (RenderEvent t) (Traversal (Render t)) Int (ReflexM t) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadWriter (Traversal (Render t)), MonadReader (RenderEvent t), MonadState Int)

instance ReflexHost t => MonadSample t (Html t) where
   sample = Html . lift . sample

instance ReflexHost t => MonadHold t (Html t) where
   hold a0 = Html . lift . hold a0


class HasDomEvent t a where
  domEvent :: EventName en -> a -> Event t (EventResultType en)

instance Reflex t => HasDomEvent t (Element t) where
  domEvent en e = unEventResult <$> select (elementEvents e) (WrapArg en)


newtype Element t = Element { elementEvents :: Events t }

freshTag :: Html t (Tag a)
freshTag = do
  i <- get
  Tag i <$ put (i + 1)

newtype Traversal f = Traversal (f ())

instance Applicative f => Monoid (Traversal f) where
  mempty = Traversal $ pure ()
  mappend (Traversal a) (Traversal b) = Traversal $ a *> b



render_ :: RenderM t => Render t () -> Html t ()
render_ = tell . Traversal

render :: RenderM t =>  Render t a -> Html t (Event t a)
render r = do
  tag <- freshTag
  tell $ Traversal $ returnRender tag r
  asks (fmapMaybe (DMap.lookup tag))


click :: Reflex t => Element t -> Event t ()
click  = domEvent Click


holdSelector :: MonadReflex t m => Event t (EventSelector t k) -> m (EventSelector t k)
holdSelector e = do
  b <- hold (EventSelector $ const never) e
  return $ EventSelector $ \k -> switch (flip select k <$> b)


element' :: RenderM t => String -> String -> Map String String -> Html t (Events t)
element' ns tag attrs = do
  es <- render $ bindEvents =<< renderEmptyElement ns tag attrs
  holdSelector es



