{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction #-}
module Reflex.Dom.Html.Attributes  where

import Reflex.Dom 

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Functor.Contravariant

import qualified Data.Map as Map
import Reflex.Dom.Html.Internal


-- Attribute binders
(-:) :: (MonadHold t m, Reflex t) => Attr a -> a -> Attribute t m
a -: v = (_attr_key a, value)
  where value = return $ StaticA (_attr_map a v)

  
(~:) :: (MonadHold t m, Reflex t) => Attr a -> Dynamic t a -> Attribute t m
a ~: v = (_attr_key a, value)
  where value = DynamicA <$> mapDyn (_attr_map a) v 


(~?) :: (MonadHold t m, Reflex t) => Attr a -> Dynamic t (Maybe a) -> Attribute t m
a ~? v = (_attr_key a, value)
  where value = DynamicA <$> mapDyn (>>= _attr_map a) v 


-- -- HTML attributes
-- width_, height_, class_, href_, style_ ::  Attr String
width_ = stringAttr "width"
height_ = stringAttr "height"
class_ = stringAttr "class"
href_ = stringAttr "href"
style_ = stringAttr "style"
for_ = stringAttr "for"
visibility_ = stringAttr "visibility"
name_ = stringAttr "name"
type_ = stringAttr "type"
placeholder_ = stringAttr "placeholder"


rows_ :: Attr Int
rows_ = showAttr "rows"

hidden_ :: Attr Bool
hidden_ = boolAttr "hidden" 


-- Convenience combiators for attributes 
toMaybe :: a -> Bool -> Maybe a
toMaybe a True = Just a
toMaybe _ _    = Nothing

classes_ :: Attr [String]
classes_ = contramap (concat . intersperse " ") $ class_

-- Concatenate attribute values together e.g. 
-- concatA [class_ :- "foo", class_ :- "bar"] == [class_ :- "foo bar"]
concatA :: MonadWidget t m => [Attribute t m] -> m [Attribute t m]
concatA attrs = forM (makeGroups attrs) $ \(k, g) -> do
  v <- concatValues g
  return (k, return v)


toggleA ::  String -> Attr String -> Attr Bool
toggleA str (Attr k m) = Attr k ((>>= m) . toMaybe str) 

styleHidden :: Attr Bool
styleHidden = toggleA "visibility:hidden" style_ 