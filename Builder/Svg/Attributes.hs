module Builder.Svg.Attributes where

import Prelude
import Builder.Attribute

import Builder.TH
import Data.Text (Text)
import qualified Data.Text as T

import Linear.V2
import Control.Lens (view)

import Data.Functor.Contravariant
import Data.Monoid

data Transform = Scale Float Float | Translate Float Float

instance Show Transform where
  show (Scale x y) = concat ["scale (", show x, " ", show y, ")"]
  show (Translate x y) = concat ["translate (", show x, " ", show y, ")"]
  
points_ = spaceSep $ showingA showPoint "points"  :: Attribute [V2 Float]
  where showPoint (V2 x y) = show x <> "," <> show y
  
  

x_         = floatA "x"                    :: Attribute Float
y_         = floatA "y"                    :: Attribute Float

width_     = floatA "width"                :: Attribute Float
height_    = floatA "height"               :: Attribute Float


href_     = strA "href"                :: Attribute Text

cx_         = floatA "cx"                  :: Attribute Float
cy_         = floatA "cy"                  :: Attribute Float

r_         = floatA "r"                    :: Attribute Float

class_     = strA "class"              :: Attribute Text
classes_     = spaceSep class_         :: Attribute [Text]

transform_ = spaceSep $ showA "transform"  :: Attribute [Transform]

version_           = strA "version"         :: Attribute Text

-- Composite Attributes

xy_ :: Attribute (V2 Float)
xy_ =  contramap (view _x) x_ <> contramap (view _y) y_

cxcy_ :: Attribute (V2 Float)
cxcy_ =  contramap (view _x) x_ <> contramap (view _y) y_

wh_ :: Attribute (V2 Float)
wh_ =  contramap (view _x) width_ <> contramap (view _y) height_  
