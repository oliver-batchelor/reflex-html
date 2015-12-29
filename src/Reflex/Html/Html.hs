
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


type Request t = Dirty t (Traversal (Render t))

newtype Html t a = Html
  { unHtml :: RSST
      (EventSelector t Tag)
      (Traversal (Builder t))
      (S.Supply, [Request t])
        (ReflexM t) a
  }
    deriving (Functor, Applicative, Monad, MonadFix)


instance ReflexHost t => MonadSample t (Html t) where
   sample = Html . lift . sample

instance ReflexHost t => MonadHold t (Html t) where
   hold a0 = Html . lift . hold a0


deriving instance Renderer t => MonadReader (EventSelector t Tag) (Html t)
deriving instance Renderer t => MonadWriter (Traversal (Builder t)) (Html t)


class HasDomEvent t a where
  domEvent :: EventName en -> a -> Event t (EventResultType en)

instance Reflex t => HasDomEvent t (Element t) where
  domEvent en e = unEventResult <$> select (elementEvents e) (WrapArg en)


newtype Element t = Element { elementEvents :: Events t }

freshTag :: Html t (Tag a)
freshTag = Html $ do
  i <- _1 %%= S.freshId
  return (Tag i)

eventTag :: Renderer t => Html t (Event t a, Tag a)
eventTag = do
  tag <- freshTag
  e <- asks (flip select tag)
  return (e, tag)

build_ :: Renderer t => Builder t () -> Html t ()
build_  = Html .  tell . Traversal

tagRender :: Renderer t => Html t (Event t a, Render t a -> Traversal (Render t))
tagRender = do
  (e, tag) <- eventTag
  return (e, Traversal . (>>= return_ tag))


render_ :: Renderer t => Event t (Render t ()) -> Html t ()
render_ e = do
  (done, f) <- tagRender
  tellRequest =<< dirty (f <$> e) done


tellRequest :: Renderer t => Dirty t (Traversal (Render t)) -> Html t ()
tellRequest d = Html $ _2 %= (d :)

build :: Renderer t =>  Builder t a -> Html t (Event t a)
build r = do
  tag <- freshTag
  tell $ Traversal (r >>= return_ tag)
  asks (flip select tag)


holdBuild :: Renderer t => Builder t a -> Html t (Behavior t (Maybe a))
holdBuild r = hold Nothing =<< fmap Just <$> build r


clicked :: Reflex t => Element t -> Event t ()
clicked  = domEvent Click


holdSelector :: MonadReflex t m => Event t (EventSelector t k) -> m (EventSelector t k)
holdSelector e = do
  b <- hold (EventSelector $ const never) e
  return $ EventSelector $ \k -> switch (flip select k <$> b)


text :: Renderer t => String -> Html t ()
text str = build_ $ void $ buildText str


dynText :: Renderer t => Dynamic t String -> Html t ()
dynText d = do
  e <- build $
    sample (current d) >>= fmap Just . buildText

  textB <- hold Nothing e
  render_ (attachWithMaybe update textB (updated d))
    where update mText str = liftIO . flip updateText str <$> mText


element :: Renderer t => String -> String -> Map String String -> Html t (Events t)
element ns tag attrs = do
  es <- build $ bindEvents =<< buildEmptyElement ns tag attrs
  holdSelector es


collectBuilder :: Renderer t => Html t a -> Html t (a, Builder t ())
collectBuilder (Html m) = Html $ do
  (a, Traversal r) <- collectRSST m
  return (a, r)

element_ :: Renderer t => String -> String -> Map String String -> Html t a -> Html t a
element_ ns tag attrs child = do
  (a, r) <- collectBuilder child
  build_ $ void $ buildElement ns tag attrs r
  return a

element' :: Renderer t => String -> String -> Map String String -> Html t a -> Html t (a, Events t)
element' ns tag attrs child = do
  (a, r) <- collectBuilder child
  es <- build $ bindEvents . fst =<< buildElement ns tag attrs r
  (a,) <$> holdSelector es


mergeRequests :: Renderer t => [Request t] -> Html t (Request t)
mergeRequests reqs = do
  (reset, tag) <- eventTag
  req <- mergeDirty reqs reset
  let occ = Traversal (return_ tag ())

  return (fmap (mappend occ) <$> req)


runReflexFrame :: ReflexM Spider a -> SpiderHost a
runReflexFrame m = runHostFrame (runReflexM m)

runHtmlFrame :: Event Spider (DMap Tag) -> Html Spider () -> IO (Builder Spider ())
runHtmlFrame e m = runSpiderHost $ do
  s <- liftIO S.newSupply
  (reqs, r) <- runReflexFrame $ evalRSST (unHtml runRoot) (fan e) (s, [])

  return (getTraversal r)
    where runRoot = m >> (Html (gets snd) >>= mergeRequests >>= holdDirty)

htmlBody :: Html Spider () -> IO ()
htmlBody html = do
  (e, ref) <- runSpiderHost newEventWithTriggerRef
  builder <- runHtmlFrame e html
  buildBody ref builder






