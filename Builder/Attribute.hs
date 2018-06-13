{-# LANGUAGE UndecidableInstances, InstanceSigs #-}

module Builder.Attribute where

import Data.Maybe (catMaybes)
import Reflex hiding (Value)
import Reflex.Active
import Reflex.Dom (AttributeName(..))

import Data.Functor
import Data.Semigroup

import Control.Applicative
import Data.Functor.Contravariant

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Dependent.Map as M
import Data.Dependent.Map (DMap, DSum(..))

import Data.Functor.Identity

import Data.GADT.Compare

import Data.Coerce
import GHC.Exts (IsList(..))


class GCompare k => AttributeSet (k :: * -> *) where
  concatA :: k a -> [a] -> a
  concatA k = foldl1 (appendA k)

  appendA :: k a -> (a -> a -> a)
  appendA k = const

  valueA  :: k a -> a -> (AttributeName, Maybe Text)


data Binding attr t a where
  Attribute :: attr a -> Binding attr t (Active t a)

type Property attr t = DSum (Binding attr t) Identity

instance (Reflex t, GEq attr) => GEq (Binding attr t) where
  geq (Attribute a) (Attribute b) = case geq a b of
    Just Refl -> Just Refl
    Nothing   -> Nothing

instance (Reflex t, GCompare attr) => GCompare (Binding attr t) where
  gcompare (Attribute a) (Attribute b) = case gcompare a b of
    GEQ -> GEQ; GLT -> GLT; GGT -> GGT

newtype PropertyMap attr t = PropertyMap { unPropertyMap :: DMap (Binding attr t) Identity }
type AttributeMap attr t = DMap attr (Active t)

attributeProps :: forall t attr. Reflex t =>  PropertyMap attr t -> AttributeMap attr t
attributeProps (PropertyMap props) = M.fromDistinctAscList (toAttr `fmapMaybe` M.toAscList props) where

  toAttr :: DSum (Binding attr t) Identity -> Maybe (DSum attr (Active t))
  toAttr (Attribute k :=> Identity v) = Just (k :=> v)


splitAttributes ::  (Reflex t, GCompare attr) => AttributeMap attr t -> (DMap attr Identity, DMap attr (Dynamic t))
splitAttributes = M.mapEitherWithKey f where
  f k (Static a) = Left (Identity a)
  f k (Dyn d)    = Right d



appendBinding :: forall t attr v. (AttributeSet attr, Reflex t)
              => Binding attr t v -> Identity v -> Identity v -> Identity v
appendBinding (Attribute k) a b = liftA2 (liftA2 (appendA k)) a b where


instance (AttributeSet attr, Reflex t) => Monoid (PropertyMap attr t) where
  mempty = PropertyMap M.empty
  mappend (PropertyMap a) (PropertyMap b) = PropertyMap $ M.unionWithKey appendBinding a b
  mconcat maps = PropertyMap $ M.mapWithKey f $ M.unionsWithKey (const mappend) lists where
    f k = Identity .  concatBindings k
    lists = M.map (pure . runIdentity) <$> (coerce maps)



concatBindings :: (Reflex t, AttributeSet attr) => Binding attr t a -> [a] -> a
concatBindings (Attribute a) as = concatAttribute a as

concatAttribute :: Reflex t => AttributeSet attr => attr a -> [Active t a] -> Active t a
concatAttribute k = fmap (concatA k) . distributeListOverActive

infixr 0 =:, ~:

(=:) :: (Reflex t) => attr a -> a -> Property attr t
(=:) k a =  attr k (Static a)

(~:) :: (Reflex t) => attr a -> Dynamic t a -> Property attr t
(~:) k d = attr k (Dyn d)

attr :: (Reflex t) => attr a -> Active t a -> Property attr t
attr k v = Attribute k :=> Identity v

mapSum :: (forall v. f v -> g v) -> DSum k f -> DSum k g
mapSum f (k :=> a) = (k :=> f a)

groupKeys :: GCompare k => [DSum k Identity] -> DMap k []
groupKeys = M.fromListWithKey (const mappend) . fmap (mapSum (pure . runIdentity))

instance (AttributeSet attr, Reflex t) => IsList (PropertyMap attr t) where
  type Item (PropertyMap attr t) = Property attr t

  fromList = PropertyMap . M.mapWithKey f . groupKeys where
    f k = Identity .  concatBindings k

  toList = M.toList . unPropertyMap 
