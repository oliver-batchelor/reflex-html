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
import Data.Text as T

import qualified Data.Dependent.Map as M
import Data.Dependent.Map (DMap, DSum(..))

import Data.Functor.Identity

import Data.GADT.Compare


import Data.Coerce
import GHC.Exts (IsList(..))

data Transform = Scale Float Float | Translate Float Float

instance Show Transform where
  show (Scale x y) = mconcat ["scale (", show x, " ", show y, ")"]
  show (Translate x y) = mconcat ["translate (", show x, " ", show y, ")"]


class GCompare k => AttributeSet (k :: * -> *) where
  concatA :: k a -> [a] -> a
  concatA k []    = error "concatA: empty list"
  concatA k (a:_) = a

  appendA :: k a -> (a -> a -> a)
  valueA  :: k a -> a -> (AttributeName, Maybe Text)


data Binding attr t a where
  Attribute :: attr a -> Binding attr t (Active t a)

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

concatBindings :: Reflex t => AttributeSet attr => Binding attr t a -> [a] -> a
concatBindings (Attribute a) as = concatAttribute a as

concatAttribute :: Reflex t => AttributeSet attr => attr a -> [Active t a] -> Active t a
concatAttribute k = fmap (concatA k) . distributeListOverActive

infixr 0 =:, ~:

(=:) :: (GCompare attr, Reflex t) => attr a -> a -> PropertyMap attr t
(=:) k a =  attr k (Static a)

(~:) :: (GCompare attr, Reflex t) => attr a -> Dynamic t a -> PropertyMap attr t
(~:) k d = attr k (Dyn d)

attr :: (GCompare attr, Reflex t) => attr a -> Active t a -> PropertyMap attr t
attr k v = PropertyMap (M.singleton (Attribute k) (Identity v))


instance (AttributeSet attr, Reflex t) => IsList (PropertyMap attr t) where
  type Item (PropertyMap attr t) = PropertyMap attr t
  fromList = mconcat
  toList x = [x]

--
-- optional :: Attribute a -> Attribute (Maybe a)
-- optional (Attribute f name) = Attribute  (>>= f) name
--
-- cond :: a -> Attribute a -> Attribute Bool
-- cond a (Attribute f name) = Attribute (\b -> if b then f a else Nothing) name
--
--
-- sepBy :: Text -> Attribute a -> Attribute [a]
-- sepBy sep (Attribute f name) = Attribute  f' name where
--   f' xs = case catMaybes (f <$> xs) of
--       []  -> Nothing
--       strs -> Just $ T.intercalate sep strs
--
-- commaSep :: Attribute a -> Attribute [a]
-- commaSep = sepBy ","
--
-- spaceSep :: Attribute a -> Attribute [a]
-- spaceSep = sepBy " "
--
--
-- showA :: Show a => Text -> Attribute a
-- showA = contramap (T.pack . show) . strA
--
-- strA :: Text -> Attribute Text
-- strA name = Attribute Just (AttributeName Nothing name)
--
-- commaListA :: Text -> Attribute [Text]
-- commaListA = commaSep . strA
--
-- spaceListA :: Text -> Attribute [Text]
-- spaceListA = spaceSep . strA
--
-- boolA :: Text -> Attribute Bool
-- boolA name = Attribute (\b -> if b then Just "" else Nothing) (AttributeName Nothing name)
--
-- intA :: Text -> Attribute Int
-- intA = showA
--
-- floatA :: Text -> Attribute Float
-- floatA = showA
--
-- ifA :: Text -> Text -> Text -> Attribute Bool
-- ifA t f = contramap fromBool . strA
--   where fromBool b = if b then t else f
--
-- styleA :: Text -> Attribute [(Text, Text)]
-- styleA name = Attribute toStyle (AttributeName Nothing name) where
--   toStyle attrs = Just $ T.concat (pair <$> attrs)
--   pair (attr, value) = attr <> ":" <> value <> ";"



  -- data Bind = D | S
  --
  --
  -- type family (B v) :: Bind where
  --   B (Dynamic t a)  = D
  --   B a              = S
  --
  --
  -- class HasProperty t v a where
  --   prop :: Attribute a -> v -> Property t
  --
  -- instance (Value b v ~ a, B v ~ b, HasBinding t b v) => HasProperty t v a where
  --   prop attr v = AttrProp attr (bind (Proxy :: Proxy b) v)
  --
  -- class HasBinding t (b :: Bind) v where
  --     type Value b v
  --     bind :: Proxy b -> v -> Binding t (Value b v)
  --
  -- instance (Reflex t) => HasBinding t D (Dynamic t a) where
  --   type Value D (Dynamic t a) = a
  --   bind _ d = (DynBinding d)
  --
  -- instance (Reflex t) => HasBinding t S a where
  --   type Value S a = a
  --   bind _ a = (StaticBinding a)
  --
  -- -- class HasProp' (dyn :: Bool) v a where
  -- --   prop' :: Proxy dyn -> v -> Binding t a
  -- default (Text, Float)
  --
  -- foo = prop (floatA "blah") 5.0
