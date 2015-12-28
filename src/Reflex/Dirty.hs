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

holdDirty :: (MonadReflex t m, Monoid a) => Dirty t a -> m (Behavior t a)
holdDirty d = join <$> hold (constant mempty) d

mergeDirty :: (MonadReflex t m, Monoid a) => [Dirty t a] -> Event t () -> m (Dirty t a)
mergeDirty cs reset = do
  d <- foldDyn ($) [] $ mergeWith (.)
    [ const mempty <$ reset
    , mappend . NE.toList <$>  mergeList cs
    ]

  fmap join <$> dirty (fmapMaybe nonEmpty (updated d)) reset

  where
    nonEmpty [] = Nothing
    nonEmpty xs = Just (mconcat xs)

