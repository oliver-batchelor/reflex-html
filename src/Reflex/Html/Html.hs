
{-# LANGUAGE ConstraintKinds, RankNTypes, GADTs, TemplateHaskell, ConstraintKinds, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Reflex.Html.Html  where

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


type Request t = Dirty t (Ap (HostFrame t) (DList (DSum Tag)))

newtype Html t a = Html
  { unHtml :: RSST
      (EventSelector t Tag)
      (Traversal (Render t))
      (S.Supply, [Request t])
        (ReflexM t) a
  }
    deriving (Functor, Applicative, Monad, MonadFix)


instance ReflexHost t => MonadSample t (Html t) where
   sample = Html . lift . sample

instance ReflexHost t => MonadHold t (Html t) where
   hold a0 = Html . lift . hold a0


deriving instance ReflexHost t => MonadReader (EventSelector t Tag) (Html t)
deriving instance ReflexHost t => MonadWriter (Traversal (Render t)) (Html t)


class HasDomEvent t a where
  domEvent :: EventName en -> a -> Event t (EventResultType en)

instance Reflex t => HasDomEvent t (Element t) where
  domEvent en e = unEventResult <$> select (elementEvents e) (WrapArg en)


newtype Element t = Element { elementEvents :: Events t }

freshTag :: Html t (Tag a)
freshTag = Html $ do
  i <- _1 %%= S.freshId
  return (Tag i)

eventTag :: RenderM t => Html t (Event t a, Tag a)
eventTag = do
  tag <- freshTag
  e <- asks (flip select tag)
  return (e, tag)

render_ :: RenderM t => Render t () -> Html t ()
render_  = Html .  tell . Traversal

tagRender :: RenderM t => Html t (Event t a, HostFrame t a -> Ap (HostFrame t) (DList (DSum Tag)))
tagRender = do
  (e, tag) <- eventTag
  return (e, Ap . (>>= tagReturn tag))

  where
    tagReturn tag a = return (pure (tag :=> a))


performRender_ :: RenderM t => Event t (HostFrame t ()) -> Html t ()
performRender_ e = do
  (reset, f) <- tagRender
  tellRequest =<< dirty (f <$> e) reset


tellRequest :: RenderM t => Dirty t (Ap (HostFrame t) (DList (DSum Tag))) -> Html t ()
tellRequest d = Html $ _2 %= (d :)

render :: RenderM t =>  Render t a -> Html t (Event t a)
render r = do
  tag <- freshTag
  tell $ Traversal $ returnRender tag r
  asks (flip select tag)



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
dynText d = do
  e <- render $
    sample (current d) >>= fmap Just . renderText

  textB <- hold Nothing e
  performRender_ (attachWithMaybe update textB (updated d))
    where update mText str = liftIO . flip updateText str <$> mText


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


mergeRequests :: RenderM t => [Request t] -> Html t (Request t)
mergeRequests reqs = do
  (reset, tag) <- eventTag
  req <- mergeDirty reqs reset
  let occ = tag :=> ()

  return (fmap (fmap (DL.cons occ)) <$> req)


runReflexFrame :: ReflexM Spider a -> SpiderHost a
runReflexFrame m = runHostFrame (runReflexM m)

runHtmlFrame :: Event Spider (DMap Tag) -> Html Spider () -> IO (Render Spider ())
runHtmlFrame e m = runSpiderHost $ do
  s <- liftIO S.newSupply
  (reqs, r) <- runReflexFrame $ evalRSST (unHtml runRoot) (fan e) (s, [])

  return (getTraversal r)
    where runRoot = m >> (Html (gets snd) >>= mergeRequests >>= holdDirty)

htmlBody :: Html Spider () -> IO ()
htmlBody html = do
  (e, ref) <- runSpiderHost newEventWithTriggerRef
  render <- runHtmlFrame e html
  renderBody ref render






