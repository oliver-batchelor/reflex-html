{-# LANGUAGE LambdaCase #-}

module Reflex.Dirty
  ( dirty
  , foldDirty
  , holdDirty
  , mergeDirty
  , Dirty

  ) where

import Data.Coerce
import Data.Functor
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Reflex.Monad

newtype Dirty t a = Dirty { unDirty :: Event t (Behavior t a) }

instance Reflex t => Functor (Dirty t) where
  fmap f = Dirty . fmap (fmap f) . unDirty


resetting :: MonadReflex t m => (a -> PushM t b) -> Event t a -> Event t () -> m (Event t b)
resetting f ea reset = do
  rec
    let eb  = gate sent $ pushAlways f ea
    sent <- hold False $ leftmost [True <$ eb, False <$ reset]
  return eb

dirty :: MonadReflex t m => Event t a -> Event t () -> m (Dirty t a)
dirty e = fmap Dirty . resetting (flip hold e) e

foldDirty :: MonadReflex t m => (a -> a -> a) -> Event t a -> Event t () -> m (Dirty t a)
foldDirty f e = fmap Dirty . resetting (fmap current . flip (foldDyn f) e) e


holdDirty :: (MonadReflex t m, Monoid a) => [Dirty t a] -> Event t () -> m (Behavior t (Maybe a))
holdDirty ds reset = do
  l <- current <$> holdDirtyList ds reset
  return $ pull $ nonEmpty <$> sample l >>= traverse sample


holdDirtyList :: MonadReflex t m => [Dirty t a] -> Event t () -> m (Dynamic t [Behavior t a])
holdDirtyList ds reset = foldDyn ($) [] $ mergeWith (.)
    [ const mempty <$ reset
    , mappend . NE.toList <$>  mergeList (coerce ds)
    ]


mergeDirty :: (MonadReflex t m, Monoid a) => [Dirty t a] -> Event t () -> m (Dirty t a)
mergeDirty ds reset = do
  l <- updated <$> holdDirtyList ds reset
  Dirty . fmap join . unDirty <$> dirty (fmapMaybe nonEmpty l) reset


nonEmpty :: Monoid a => [a] -> Maybe a
nonEmpty [] = Nothing
nonEmpty xs = Just (mconcat xs)

