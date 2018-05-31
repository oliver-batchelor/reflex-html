{-# LANGUAGE UndecidableInstances, InstanceSigs #-}

module Builder.Attribute where

import Data.Maybe (catMaybes)
import Reflex hiding (Value)
import Reflex.Dom (AttributeName(..))

import Data.Functor
import Data.Monoid
import Data.Functor.Contravariant
import Data.Text as T


data Attribute a = Attribute { attrConvert :: a -> Maybe Text, attrName :: !AttributeName }

instance Contravariant Attribute where
  contramap f' (Attribute f name) = Attribute (f . f') name

data Binding t a where
  StaticBinding :: a -> Binding t a
  DynBinding    :: Dynamic t a -> Binding t a


data Property t where
  AttrProp :: Attribute a -> Binding t a -> Property t


infixr 0 =:, ~:

(=:) :: Attribute a -> a -> Property t
(=:) attr a = AttrProp attr (StaticBinding a)

(~:) :: Attribute a -> Dynamic t a -> Property t
(~:) attr d = AttrProp attr (DynBinding d)



setNs :: Text -> Attribute a -> Attribute a
setNs ns (Attribute f (AttributeName _ name)) = Attribute f (AttributeName (Just ns) name)

optional :: Attribute a -> Attribute (Maybe a)
optional (Attribute f name) = Attribute  (>>= f) name

cond :: a -> Attribute a -> Attribute Bool
cond a (Attribute f name) = Attribute (\b -> if b then f a else Nothing) name


sepBy :: Text -> Attribute a -> Attribute [a]
sepBy sep (Attribute f name) = Attribute  f' name where
  f' xs = case catMaybes (f <$> xs) of
      []  -> Nothing
      strs -> Just $ T.intercalate sep strs

commaSep :: Attribute a -> Attribute [a]
commaSep = sepBy ","

spaceSep :: Attribute a -> Attribute [a]
spaceSep = sepBy " "


showA :: Show a => Text -> Attribute a
showA = contramap (T.pack . show) . strA

strA :: Text -> Attribute Text
strA name = Attribute Just (AttributeName Nothing name)

commaListA :: Text -> Attribute [Text]
commaListA = commaSep . strA

spaceListA :: Text -> Attribute [Text]
spaceListA = spaceSep . strA

boolA :: Text -> Attribute Bool
boolA name = Attribute (\b -> if b then Just "" else Nothing) (AttributeName Nothing name)

intA :: Text -> Attribute Int
intA = showA

floatA :: Text -> Attribute Float
floatA = showA

ifA :: Text -> Text -> Text -> Attribute Bool
ifA t f = contramap fromBool . strA
  where fromBool b = if b then t else f

styleA :: Text -> Attribute [(Text, Text)]
styleA name = Attribute toStyle (AttributeName Nothing name) where
  toStyle attrs = Just $ T.concat (pair <$> attrs)
  pair (attr, value) = attr <> ":" <> value <> ";"



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
