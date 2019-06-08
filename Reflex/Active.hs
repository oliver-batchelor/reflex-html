module Reflex.Active where

import Reflex
import Data.String
import Data.Default

import qualified Data.Map as M
import Data.Map (Map)

import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)

import Data.Maybe (fromJust)
import Control.Applicative
import Data.Foldable (toList)

-- Active data type for mixing Dynamics and static values
-- originally from: https://github.com/tomsmalley/semantic-reflex/
-- by Tom Smalley

data Active t a
  = Static a
  | Dyn (Dynamic t a)

deriving instance Reflex t => Functor (Active t)

instance Reflex t => Applicative (Active t) where
  pure = Static
  Static f <*> Static a = Static (f a)
  Static f <*> Dyn a = Dyn (pure f <*> a)
  Dyn f <*> Static a = Dyn (f <*> pure a)
  Dyn f <*> Dyn a = Dyn (f <*> a)


instance (Reflex t, Semigroup a) => Semigroup (Active t a) where
  Static a <> Static b = Static (a <> b)
  Static a <> Dyn b = Dyn (pure a <> b)
  Dyn a <> Static b = Dyn (a <> pure b)
  Dyn a <> Dyn b = Dyn (a <> b)
  sconcat = fmap sconcat . activeNonEmpty

instance (Reflex t, Monoid a) => Monoid (Active t a) where
  mempty = Static mempty
  mappend = (<>)
  mconcat = fmap mconcat . activeList

instance (Reflex t, Num a) => Num (Active t a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  fromInteger = pure . fromInteger
  abs = fmap abs
  signum = fmap signum


instance IsString a => IsString (Active t a) where
  fromString = Static . fromString

instance Default a => Default (Active t a) where
  def = Static def

splitActives :: Ord k => Map k (Active t a) -> (Map k a, Map k (Dynamic t a))
splitActives = M.mapEither $ \case
  Static a -> Left a
  Dyn d    -> Right d

activeMap :: (Reflex t, Ord k) => Map k (Active t a) -> Active t (Map k a)
activeMap as = if null dynamics
    then Static consts
    else Dyn (mappend consts <$> distributeMapOverDynPure dynamics)
  where
    (consts, dynamics) = splitActives as

activeList :: Reflex t => [Active t a] -> Active t [a]
activeList actives = M.elems <$> activeMap m
    where m = M.fromList (zip [0..] actives)

activeNonEmpty :: Reflex t => NonEmpty (Active t a) -> Active t (NonEmpty a)
activeNonEmpty = fmap (fromJust . nonEmpty) . activeList . toList

