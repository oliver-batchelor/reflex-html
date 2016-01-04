{-# LANGUAGE LambdaCase #-}

module Reflex.Dirty
  ( dirty
  , holdDirty
  , mergeDirty
  , Dirty

  ) where

import Data.Functor
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Reflex.Monad

type Dirty t a = Event t (Behavior t a)


dirty :: MonadReflex t m => Event t a -> Event t () -> m (Dirty t a)
dirty e reset = do
  sent <- hold False $ leftmost [True <$ e, False <$ reset]
  return $ flip push e $ \a -> do
    sample sent >>= \case
      False -> Just <$> hold a e
      True  -> return Nothing

holdDirty :: (MonadReflex t m, Monoid a) => [Dirty t a] -> Event t () -> m (Behavior t (Maybe a))
holdDirty ds reset = do
  l <- current <$> holdDirtyList ds reset
  return $ pull $ nonEmpty <$> sample l >>= traverse sample


holdDirtyList :: MonadReflex t m => [Dirty t a] -> Event t () -> m (Dynamic t [Behavior t a])
holdDirtyList ds reset = foldDyn ($) [] $ mergeWith (.)
    [ const mempty <$ reset
    , mappend . NE.toList <$>  mergeList ds
    ]


mergeDirty :: (MonadReflex t m, Monoid a) => [Dirty t a] -> Event t () -> m (Dirty t a)
mergeDirty ds reset = do
  l <- updated <$> holdDirtyList ds reset
  fmap join <$> dirty (fmapMaybe nonEmpty l) reset


nonEmpty :: Monoid a => [a] -> Maybe a
nonEmpty [] = Nothing
nonEmpty xs = Just (mconcat xs)

