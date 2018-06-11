module Builder.Svg.Attributes where

import Prelude
import Builder.Attribute

import Builder.TH
import Data.Text (Text)
import Data.GADT.Compare.TH

data SvgAttribute a where
  X :: SvgAttribute Bool
  Y  :: SvgAttribute [Text]
  Style  :: SvgAttribute [(Text, Text)]
  Custom :: Text -> SvgAttribute Text

deriveGEq ''SvgAttribute
deriveGCompare ''SvgAttribute

--
--
-- x_         = floatA "x"                    :: Attribute Float
-- y_         = floatA "y"                    :: Attribute Float
--
-- width_     = floatA "width"                :: Attribute Float
-- height_    = floatA "height"               :: Attribute Float
--
--
-- href_     = strA "href"                :: Attribute Text
--
-- cx_         = floatA "cx"                  :: Attribute Float
-- cy_         = floatA "cy"                  :: Attribute Float
--
-- r_         = floatA "r"                    :: Attribute Float
-- class_           = strA "class"                      :: Attribute Text
-- classes_           = spaceListA "class"              :: Attribute [Text]
--
-- transform_ = spaceSep $ showA "transform"  :: Attribute [Transform]
--
-- version_           = strA "version"         :: Attribute Text
