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
import Data.GADT.Compare.TH

import Data.Coerce
import GHC.Exts (IsList(..))

data Transform = Scale Float Float | Translate Float Float

instance Show Transform where
  show (Scale x y) = mconcat ["scale (", show x, " ", show y, ")"]
  show (Translate x y) = mconcat ["translate (", show x, " ", show y, ")"]


data AttrType a where
  FloatA      :: AttrType Float
  IntA        :: AttrType Int
  TextA       :: AttrType Text
  BoolA       :: AttrType Bool
  StyleA      :: AttrType (Text, Text)
  ListA       :: Char -> AttrType a -> AttrType [a]

  -- Svg specific
  TransformA  :: AttrType Transform

deriveGCompare ''AttrType
deriveGEq ''AttrType

data Attribute a where
  Attr :: !AttributeName -> AttrType a -> Attribute a

instance GCompare Attribute where
  gcompare (Attr name t) (Attr name' t') = runGComparing $ do
     compare' name name'
     let Refl = case gcompare x y of
           GEQ -> Refl
           _   -> error ("conflicting types specified for attribute: " <> show name)
     return GEQ

deriveGEq ''Attribute


data Binding t a where
  AttrB :: Attribute a -> Binding t (Active t a)

instance Reflex t => GEq (Binding t) where
  geq (AttrB a) (AttrB b) = case geq a b of
    Just Refl -> Just Refl
    Nothing   -> Nothing


instance Reflex t => GCompare (Binding t) where
  gcompare (AttrB a) (AttrB b) = case gcompare a b of
    GEQ -> GEQ; GLT -> GLT; GGT -> GGT


newtype PropertyMap t = PropertyMap { unAttr :: DMap (Binding t) Identity }

type AttributeMap t = DMap Attribute (Active t)

attributes :: forall t. Reflex t =>  PropertyMap t -> AttributeMap t
attributes (PropertyMap props) = M.fromDistinctAscList (toAttr `fmapMaybe` M.toAscList props) where

  toAttr :: DSum (Binding t) Identity -> Maybe (DSum Attribute (Active t))
  toAttr (AttrB k :=> Identity v) = Just (k :=> v)

splitAttributes ::  Reflex t => AttributeMap t -> (DMap Attribute Identity, DMap Attribute (Dynamic t))
splitAttributes = M.mapEitherWithKey f where
  f k (Static a) = Left (Identity a)
  f k (Dyn d)    = Right d



appendBinding :: forall t v. Reflex t
              => Binding t v -> Identity v -> Identity v -> Identity v
appendBinding (AttrB k) a b = liftA2 (appendAttr k) a b


appendAttr :: forall t a. Reflex t
              => Attribute a -> Active t a -> Active t a -> Active t a
appendAttr (Attr _ (ListA _ _)) a b = a `mappend` b
appendAttr _ a b = b

instance Reflex t => Monoid (PropertyMap t) where
  mempty = PropertyMap M.empty
  mappend (PropertyMap a) (PropertyMap b) = PropertyMap $ M.unionWithKey appendBinding a b

infixr 0 =:, ~:

(=:) :: Reflex t => Attribute a -> a -> PropertyMap t
(=:) k a =  attr k (Static a)

(~:) :: Reflex t => Attribute a -> Dynamic t a -> PropertyMap t
(~:) k d = attr k (Dyn d)

attr :: Reflex t => Attribute a -> Active t a -> PropertyMap t
attr k v = PropertyMap (M.singleton (AttrB k) (Identity v))


instance Reflex t => IsList (PropertyMap t) where
  type Item (PropertyMap t) = PropertyMap t
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
