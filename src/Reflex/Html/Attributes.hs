module Reflex.Html.Attributes  
  ( module Reflex.Html.Attributes
  , Attributes
  , Attr 
  ) where
  

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Functor.Contravariant

import qualified Data.Map as Map
import Reflex.Html.Internal.Attributes
import Reflex.Html.Internal.Host


-- Attribute binders
(-:) :: (MonadHold t m, Reflex t) => Attr a -> a -> Attributes t m
a -: v = attr (_attr_key a, value)
  where value = StaticA (_attr_map a v)

  
(~:) :: (MonadHold t m, Reflex t) => Attr a -> Dynamic t a -> Attributes t m
a ~: v = attr (_attr_key a, value)
  where value = DynamicA $ mapDyn (_attr_map a) v 
 

(~?) :: (MonadHold t m, Reflex t) => Attr a -> Dynamic t (Maybe a) -> Attributes t m
a ~? v = attr (_attr_key a, value)
  where value = DynamicA $ mapDyn (>>= _attr_map a) v 


infixr 7 -:
infixr 7 ~:
infixr 7 ~?
  
-- -- HtmlT attributes
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
value_ = stringAttr "value"


rows_ :: Attr Int
rows_ = showAttr "rows"

hidden_, selected_ :: Attr Bool
hidden_ = boolAttr "hidden" 
selected_ = boolAttr "selected" 


-- Convenience combiators for attributes 
toMaybe :: a -> Bool -> Maybe a
toMaybe a True = Just a
toMaybe _ _    = Nothing

  

classes_ :: Attr [String]
classes_ = contramap (concat . intersperse " ") $ class_



toggleA ::  String -> Attr String -> Attr Bool
toggleA str (Attr k m) = Attr k ((>>= m) . toMaybe str) 

styleHidden :: Attr Bool
styleHidden = toggleA "visibility:hidden" style_ 