module Builder.Svg.Attributes 
  ( module Builder.Svg.Attributes
  , Transform(..)
  ) where

import Prelude
import Builder.Attribute

import Builder.TH.Attribute
import Data.Text (Text)
import qualified Data.Text as T

import Data.GADT.Compare.TH

import Builder.Svg.Types


$(mkAttrs "SvgAttribute" 
  [ floatA "accent-height"
  , ifA "accumulate" ("sum", "none")
  , ifA "additive" ("sum", "replace")
  , strA "alignment-baseline"
  , strA "allowReorder"
  , floatA "alphabetic"
  , floatA "amplitude"
  , strA "arabic-form"
  , floatA "ascent"
  , strA "attributeName"
  , strA "attributeType"
  , floatA "azimuth"

  , vector2A "baseFrequency"
  , strA "baseline-shift"
  , strA "baseProfile"
  , vector4A "bbox"
  , semicolonSep $ strA "begin"
  , floatA "bias"
  , strA "by"

  , strA "calcMode"
  , floatA "cap-height"
  , spaceSep $ strA "class"
  , strA "clip"
  , strA "clipPathUnits"
  , strA "clip-path"
  , strA "clip-rule"
  , strA "color"
  , strA "color-interpolation"
  , strA "color-interpolation-filters"
  , strA "color-profile"
  , strA "color-rendering"
  , strA "contentScriptType"
  , strA "contentStyleType"
  , strA "cursor"
  , floatA "cx"
  , floatA "cy"

  , strA "d"
  , strA "decelerate"
  , floatA "descent"
  , floatA "diffuseConstant"
  , strA "direction"
  , strA "display"
  , floatA "divisor"
  , strA "dominant-baseline"
  , strA "dur"
  , lengthA "dx"
  , lengthA "dy"

  , strA "edgeMode"
  , floatA "elevation"
  , strA "enable-background"
  , semicolonSep $ strA "end"
  , floatA "exponent"
  , strA "externalResourcesRequired"

  , strA "fill"
  , strA "fill-opacity"
  , strA "fill-rule"
  , strA "filter"
  , strA "filterUnits"
  , strA "flood-color"
  , strA "flood-opacity"
  , strA "font-family"
  , strA "font-size"
  , strA "font-size-adjust"
  , strA "font-stretch"
  , strA "font-style"
  , strA "font-variant"
  , strA "font-weight"
  , strA "format"
  , strA "from"
  , floatA "fr"
  , floatA "fx"
  , floatA "fy"

  , commaSep $ strA "g1"
  , commaSep $ strA "g2"
  , strA "glyph-name"
  , strA "glyph-orientation-horizontal"
  , strA "glyph-orientation-vertical"
  , strA "glyphRef"
  , transformA "gradientTransform"
  , strA "gradientUnits"

  , floatA "hanging"
  , floatA "height"
  , strA "href"
  , floatA "horiz-adv-x"
  , floatA "horiz-origin-x"

  , strA "id"
  , floatA "ideographic"
  , strA "image-rendering"
  , strA "in"
  , strA "in2"
  , floatA "intercept"

  , floatA "k"
  , floatA "k1"
  , floatA "k2"
  , floatA "k3"
  , floatA "k4"
  , strA "kernelMatrix"
  , strA "kernelUnitLength"
  , strA "kerning"
  , semicolonSep $ floatA "keyPoints"
  , strA "keySplines"
  , semicolonSep $ floatA "keyTimes"

  , strA "lang"
  , strA "lengthAdjust"
  , strA "letter-spacing"
  , strA "lighting-color"
  , floatA "limitingConeAngle"
  , strA "local"

  , strA "marker-end"
  , strA "marker-mid"
  , strA "marker-start"
  , strA "markerHeight"
  , strA "markerUnits"
  , strA "markerWidth"
  , strA "mask"
  , strA "maskContentUnits"
  , strA "maskUnits"
  , floatA "mathematical"
  , strA "max"
  , strA "media"
  , strA "method"
  , strA "min"
  , strA "mode"

  , strA "name"
  , intA "numOctaves"

  , strA "offset"
  , strA "opacity"
  , strA "operator"
  , strA "order"
  , strA "orient"
  , strA "orientation"
  , strA "origin"
  , strA "overflow"
  , floatA "overline-position"
  , floatA "overline-thickness"

  , intA "panose-1"
  , strA "paint-order"
  , strA "path"
  , floatA "pathLength"
  , strA "patternContentUnits"
  , strA "patternTransform"
  , strA "patternUnits"
  , strA "pointer-events"
  , spaceSep $ vector2A "points"
  , floatA "pointsAtX"
  , floatA "pointsAtY"
  , floatA "pointsAtZ"
  , ifA "preserveAlpha" ("true", "false")
  , ifA "preserveAspectRatio" ("true", "false") 
  , strA "primitiveUnits"

  , floatA "r"
  , floatA "radius"
  , floatA "refX"
  , floatA "refY"
  , strA "rendering-intent"
  , maybeA "indefinite" $ intA "repeatCount"
  , strA "repeatDur"
  , strA "requiredExtensions"
  , spaceSep $ stringA "requiredFeatures"
  , strA "restart"
  , strA "result"
  , floatA "rx"
  , floatA "ry"
  
  , floatA "scale"
  , intA "seed"
  , strA "shape-rendering" -- auto | optimizeSpeed | crispEdges | geometricPrecision | inherit
  , floatA "slope"
  , strA "spacing" -- auto | exact
  , floatA "specularConstant"
  , floatA "specularExponent"
  , strA "spreadMethod"
  , strA "startOffset"
  , strA "stdDeviation"
  , floatA "stemh"
  , floatA "stemv"
  , strA "stitchTiles"
  , strA "stop-color"
  , strA "stop-opacity"
  , floatA "strikethrough-position"
  , floatA "strikethrough-thickness"
  , strA "string"
  , strA "stroke"
  , strA "stroke-dasharray"
  , strA "stroke-dashoffset"
  , strA "stroke-linecap"
  , strA "stroke-linejoin"
  , strA "stroke-miterlimit"
  , strA "stroke-opacity"
  , strA "stroke-width"
  , strA "style"
  , strA "surfaceScale"
  , strA "systemLanguage"
  
  , strA "text-rendering"
  , strA "textLength"
  , strA "to"
  , transformA "transform"
  , strA "type"

  , strA "tabindex"
  , strA "tableValues"
  , strA "target"
  , strA "targetX"
  , strA "targetY"
  , strA "text-anchor"
  , strA "text-decoration"
  
  , strA "u1"
  , strA "u2"
  , floatA "underline-position"
  , floatA "underline-thickness"
  , strA "unicode"
  , strA "unicode-bidi"
  , strA "unicode-range"
  , floatA "units-per-em"
  
  , floatA "v-alphabetic"
  , floatA "v-hanging"
  , floatA "v-ideographic"
  , floatA "v-mathematical"
  , strA "values"
  , strA "version"
  , strA "vert-adv-y"
  , strA "vert-origin-x"
  , strA "vert-origin-y"
  , strA "viewBox"
  , strA "viewTarget"
  , strA "visibility"
  
  , strA "width"
  , strA "widths"
  , strA "word-spacing"
  , strA "writing-mode"
  , floatA "x"
  , floatA "x-height"
  , floatA "x1"
  , floatA "x2"
  , strA "xChannelSelector"
  
  , -- xlink
  , strA "actuate"
  , strA "arcrole"
  , strA "href"
  , strA "role"
  , strA "show"
  , strA "title"
  , strA "type"

  , -- xml
  , strA "base"
  , strA "lang"
  , strA "space"

  , strA "y"
  , strA "y1"
  , strA "y2"
  , strA "yChannelSelector"
  , strA "z"
  , strA "zoomAndPan"
  ])


deriveGEq ''SvgAttribute
deriveGCompare ''SvgAttribute

--"
--"
-- x_         = floatA "x"                    :: Attribute Float
-- y_         = floatA "y"                    :: Attribute Float
--
-- width_     = floatA "width"                :: Attribute Float
-- h"eight_    = floatA "height"               :: Attribute Float
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
