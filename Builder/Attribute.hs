{-# LANGUAGE UndecidableInstances, InstanceSigs #-}

module Builder.Attribute where

import Data.Maybe (catMaybes, fromMaybe)
import Reflex hiding (Value)
import Reflex.Dom (AttributeName(..))

import Data.Functor
import Data.Monoid
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Text as T

import qualified Data.Map as M
import Data.Map (Map)

import Control.Lens hiding (chosen)

import Reflex.Active
import Data.Coerce

newtype Attribute a
  = Attribute { unAttr :: Map AttributeName (a -> Maybe Text) } deriving Monoid

instance Contravariant Attribute where
  contramap f' (Attribute m) = Attribute (fmap (. f') m)

instance Divisible Attribute where
  divide f ab ac = contramap (fst . f) ab <> contramap (snd . f) ac
  conquer = Attribute mempty


instance Decidable Attribute where
  choose f' (Attribute mb) (Attribute mc) = Attribute $ fmap lefts mb <> fmap rights mc  where
    lefts f = (>>= f) . preview _Left . f'
    rights f = (>>= f) . preview _Right . f'

  lose _ =  Attribute mempty

_attrs :: Traversal (Attribute a) (Attribute b) (Map AttributeName (a -> Maybe Text)) (Map AttributeName (b -> Maybe Text))
_attrs = coerced

_attr :: Traversal (Attribute a) (Attribute b) (a -> Maybe Text) (b -> Maybe Text)
_attr = _attrs . traverse


optional :: Attribute a -> Attribute (Maybe a)
optional = over _attr (\f a -> a >>= f)

toggles :: Attribute a -> Attribute b -> Attribute (Either a b)
toggles = chosen

(<+>) :: Divisible f => f a -> f a -> f a
(<+>) = divide (\x -> (x, x))

applyAttr ::  Attribute a -> a -> Map AttributeName (Maybe Text)
applyAttr (Attribute m) a = M.map ($ a) m

applyAttr' ::  Attribute a -> a -> Map AttributeName Text
applyAttr' (Attribute m) a = M.mapMaybe ($ a) m


-- Property values define attributes and modifiers
-- e.g. event filters setters which operate on elements
data Property t where
  AttrProp :: Attribute a -> Active t a -> Property t

(=:) :: Reflex t => Attribute a -> a -> Property t
(=:) k a = k ~: Static a

class Reflex t => ActiveValue v t where
  (~:) :: Attribute a -> v t a -> Property t

instance Reflex t => ActiveValue Active t where
  (~:) = AttrProp

instance Reflex t => ActiveValue Dynamic t where
  (~:) k = AttrProp k . Dyn


infixr 0 =:
infixr 0 ~:


setNs :: Text -> Attribute a -> Attribute a
setNs ns = over _attrs (M.mapKeys setNs')
  where setNs' (AttributeName _ name) = AttributeName (Just ns) name

cond :: a -> Attribute a -> Attribute Bool
cond a = over _attr (\f b -> if b then f a else Nothing)

sepBy :: Text -> Attribute a -> Attribute [a]
sepBy sep = over _attr cat where
  cat f xs = case catMaybes (f <$> xs) of
    []  -> Nothing
    strs -> Just $ T.intercalate sep strs

commaSep :: Attribute a -> Attribute [a]
commaSep = sepBy ","

spaceSep :: Attribute a -> Attribute [a]
spaceSep = sepBy " "

commaListA :: Text -> Attribute [Text]
commaListA = commaSep . strA

spaceListA :: Text -> Attribute [Text]
spaceListA = spaceSep . strA

attrWith :: AttributeName -> (a -> Maybe Text) -> Attribute a
attrWith name = Attribute . M.singleton name

strA :: Text -> Attribute Text
strA name = attrWith (AttributeName Nothing name) Just

showA :: Show a => Text -> Attribute a
showA = contramap (T.pack . show) . strA


showingA :: (a -> String) -> Text -> Attribute a
showingA f = contramap (T.pack . f) . strA

boolA :: Text -> Attribute Bool
boolA name = attrWith (AttributeName Nothing name) (\b -> if b then Just "" else Nothing)

intA :: Text -> Attribute Int
intA = showA

floatA :: Text -> Attribute Float
floatA = showA

ifA :: Text -> Text -> Text -> Attribute Bool
ifA t f = contramap fromBool . strA
  where fromBool b = if b then t else f

styleA :: Text -> Attribute [(Text, Text)]
styleA name = attrWith (AttributeName Nothing name) toStyle where
  toStyle attrs = Just $ T.concat (pair <$> attrs)
  pair (attr, value) = attr <> ":" <> value <> ";"
  
  
