
{-# LANGUAGE RankNTypes, GADTs, TemplateHaskell, ConstraintKinds, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Reflex.Html.Html  where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.RSS.Strict

import Control.Lens hiding (Traversal)

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum

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

import Reflex.Html.Event
import Reflex.Html.Render
import Data.Unsafe.Tag

type Frame = Int
type NeedRender t = Behavior t (Frame, Render t (Maybe (DSum Tag)))

newtype Html t a = Html
  { unDom :: RSST
      (RenderEvent t, Behavior t Frame)
      (Traversal (Render t))
      (Int, [NeedRender t])
      (ReflexM t) a
  }
    deriving (Functor, Applicative, Monad, MonadFix)

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
freshTag = Html $ do
  i <- gets fst
  _1 .= i + 1
  return (Tag i)

newtype Traversal f = Traversal { runTraversal :: f () }

instance Applicative f => Monoid (Traversal f) where
  mempty = Traversal $ pure ()
  mappend (Traversal a) (Traversal b) = Traversal $ a *> b



render_ :: RenderM t => Render t () -> Html t ()
render_ = Html . tell . Traversal

render :: RenderM t =>  Render t a -> Html t (Event t a)
render r = do
  tag <- freshTag
  Html $ do
    tell $ Traversal $ returnRender tag r
    asks (fmapMaybe (DMap.lookup tag) . fst)

holdRender :: RenderM t => Render t a -> Html t (Behavior t (Maybe a))
holdRender r = hold Nothing =<< fmap Just <$> render r


clicked :: Reflex t => Element t -> Event t ()
clicked  = domEvent Click


holdSelector :: MonadReflex t m => Event t (EventSelector t k) -> m (EventSelector t k)
holdSelector e = do
  b <- hold (EventSelector $ const never) e
  return $ EventSelector $ \k -> switch (flip select k <$> b)


text :: RenderM t => String -> Html t ()
text str = render_ $ void $ renderText str


dynText :: RenderM t => Dynamic t String -> Html t ()
dynText d = undefined
  --do
--  eb <- holdRender $ renderText =<< sample (current d)



element :: RenderM t => String -> String -> Map String String -> Html t (Events t)
element ns tag attrs = do
  es <- render $ bindEvents =<< renderEmptyElement ns tag attrs
  holdSelector es


collectRender :: RenderM t => Html t a -> Html t (a, Render t ())
collectRender (Html m) = Html $ do
  (a, Traversal r) <- collectRSST m
  return (a, r)

element_ :: RenderM t => String -> String -> Map String String -> Html t a -> Html t a
element_ ns tag attrs child = do
  (a, r) <- collectRender child
  render_ $ void $ renderElement ns tag attrs r
  return a

element' :: RenderM t => String -> String -> Map String String -> Html t a -> Html t (a, Events t)
element' ns tag attrs child = do
  (a, r) <- collectRender child
  es <- render $ bindEvents . fst =<< renderElement ns tag attrs r
  (a,) <$> holdSelector es



runHtmlFrame :: Html Spider () -> IO (Render Spider ())
runHtmlFrame (Html m) = runSpiderHost $ do
  (renderE, ref) <- newEventWithTriggerRef
  frame <- current <$> count renderE

  (_, Traversal render) <- runHostFrame $ runReflexM $ evalRSST m (renderE, frame) (0, [])
  return (postResult ref render)


htmlBody :: Html Spider () -> IO ()
htmlBody html = do
  render <- runHtmlFrame html
  renderBody render






