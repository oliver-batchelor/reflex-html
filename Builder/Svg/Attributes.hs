module Builder.Svg.Attributes where

import Prelude
import Builder.Attribute

import Builder.TH
import Data.Text (Text)


data Transform = Scale Float Float | Translate Float Float

instance Show Transform where
  show (Scale x y) = concat ["scale (", show x, " ", show y, ")"]
  show (Translate x y) = concat ["translate (", show x, " ", show y, ")"]

x_         = floatA "x"                    :: Attribute Float
y_         = floatA "y"                    :: Attribute Float

width_     = floatA "width"                :: Attribute Float
height_    = floatA "height"               :: Attribute Float


href_     = strA "href"                :: Attribute Text

cx_         = floatA "cx"                  :: Attribute Float
cy_         = floatA "cy"                  :: Attribute Float

r_         = floatA "r"                    :: Attribute Float
classes_     = spaceSep $ strA "class"              :: Attribute [Text]

transform_ = spaceSep $ showA "transform"  :: Attribute [Transform]

version_           = strA "version"         :: Attribute Text
