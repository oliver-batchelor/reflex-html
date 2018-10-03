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

data PathCommand
  = M   Float Float
  | M'  Float Float
  | L   Float Float
  | L'  Float Float
  | V   Float
  | V'  Float
  | H   Float
  | H'  Float
  | B   Float Float Float Float Float Float
  | B'  Float Float Float Float Float Float
  | S   Float Float Float Float
  | S'  Float Float Float Float

  | Q   Float Float Float Float
  | Q'  Float Float Float Float

  | T   Float Float
  | T'  Float Float

  | Z

instance Show Transform where
  show (Scale x y) = concat ["scale (", show x, " ", show y, ")"]
  show (Translate x y) = concat ["translate (", show x, " ", show y, ")"]


instance Show PathCommand where
  show (M  x y) = concat ["M ", show x, " ", show y]
  show (M' x y) = concat ["m ", show x, " ", show y]
  show (L  x y) = concat ["L ", show x, " ", show y]
  show (L' x y) = concat ["r ", show x, " ", show y]
  show (H  d) = "H " <> show d
  show (H' d) = "h " <> show d

  show (V  d) = "V " <> show d
  show (V' d) = "v " <> show d

  show (B  x1 y1 x2 y2 x y) = concat ["B ", show x1, " ", show y1, ",", show x2, " ", show y2, ",", show x, " ", show y]
  show (B' x1 y1 x2 y2 x y) = concat ["b ", show x1, " ", show y1, ",", show x2, " ", show y2, ",", show x, " ", show y]

  show (S  x2 y2 x y) = concat ["S ", show x2, " ", show y2, ",", show x, " ", show y]
  show (S' x2 y2 x y) = concat ["s ", show x2, " ", show y2, ",", show x, " ", show y]

  show (Q  x1 y1 x y) = concat ["Q ", show x1, " ", show y1, ",", show x, " ", show y]
  show (Q' x1 y1 x y) = concat ["q ", show x1, " ", show y1, ",", show x, " ", show y]

  show (T  x y) = concat ["T ", show x, " ", show y]
  show (T' x y) = concat ["t ", show x, " ", show y]

  show Z = "Z"

d_ = spaceSep $ showA "d"  :: Attribute [PathCommand]


points_ = spaceSep $ showingA showPoint "points"  :: Attribute [V2 Float]
  where showPoint (V2 x y) = show x <> "," <> show y


type Style = (Text, Text)

style_           = styleA "style"                    :: Attribute [Style]

x_         = floatA "x"                    :: Attribute Float
y_         = floatA "y"                    :: Attribute Float

x1_         = floatA "x1"                    :: Attribute Float
y1_         = floatA "y1"                    :: Attribute Float

x2_         = floatA "x2"                    :: Attribute Float
y2_         = floatA "y2"                    :: Attribute Float

width_     = floatA "width"                :: Attribute Float
height_    = floatA "height"               :: Attribute Float

draggable_       = ifA "true" "false" "draggable" :: Attribute Bool

href_     = strA "href"                :: Attribute Text
type_     = strA "type"                :: Attribute Text
filter_     = strA "filter"                :: Attribute Text


cx_         = floatA "cx"                  :: Attribute Float
cy_         = floatA "cy"                  :: Attribute Float

r_         = floatA "r"                    :: Attribute Float


amplitude_         = floatA "amplitude"              :: Attribute Float
exponent_         = floatA "exponent"                :: Attribute Float
offset_         = floatA "offset"                    :: Attribute Float


slope_         = floatA "slope"                    :: Attribute Float
intercept_         = floatA "intercept"            :: Attribute Float


class_     = strA "class"              :: Attribute Text
classes_     = spaceSep class_         :: Attribute [Text]

clip_path_     = strA "clip-path"              :: Attribute Text
mask_     = strA "mask"              :: Attribute Text

fill_         = strA "fill"                        :: Attribute Text
stroke_         = strA "stroke"                    :: Attribute Text

pointer_events_         = strA "pointer-events"    :: Attribute Text


transform_ = spaceSep $ showA "transform"  :: Attribute [Transform]

version_           = strA "version"         :: Attribute Text

id_              = strA "id"                         :: Attribute Text
hidden_          = boolA "hidden"                    :: Attribute Bool
-- Composite Attributes

xy_ :: Attribute (V2 Float)
xy_ =  contramap (view _x) x_ <> contramap (view _y) y_


xy1_ :: Attribute (V2 Float)
xy1_ =  contramap (view _x) x1_ <> contramap (view _y) y1_

xy2_ :: Attribute (V2 Float)
xy2_ =  contramap (view _x) x2_ <> contramap (view _y) y2_


cxcy_ :: Attribute (V2 Float)
cxcy_ =  contramap (view _x) cx_ <> contramap (view _y) cy_

wh_ :: Attribute (V2 Float)
wh_ =  contramap (view _x) width_ <> contramap (view _y) height_

shown_ :: Attribute Bool
shown_ = contramap not hidden_
