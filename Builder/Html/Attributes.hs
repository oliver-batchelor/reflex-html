module Builder.Html.Attributes where

import Prelude

import Builder.Attribute
import Builder.TH

import Data.Text (Text)
import Reflex

accept_          = commaListA "accept"               :: Attribute [Text]
accept_charset_  = spaceListA "accept-charset"       :: Attribute [Text]
accesskey_       = strA "accesskey"                  :: Attribute Text
align_           = strA "align"                      :: Attribute Text
action_          = strA "action"                     :: Attribute Text
alt_             = strA "alt"                        :: Attribute Text
async_           = boolA "async"                     :: Attribute Bool
autocomplete_    = ifA "on" "off" "autocomplete"     :: Attribute Bool -- False = "off", True = "on"
autofocus_       = boolA "autofocus"                 :: Attribute Bool
autoplay_        = boolA "autoplay"                  :: Attribute Bool
autosave_        = boolA "autosave"                  :: Attribute Bool
border_          = ifA "1" "" "border"               :: Attribute Bool   -- False = "", True = "1"
challenge_       = strA "challenge"                  :: Attribute Text
charset_         = strA "charset"                    :: Attribute Text
checked_         = boolA "checked"                   :: Attribute Bool
cite_            = strA "cite"                       :: Attribute Text
class_           = strA "class"                      :: Attribute Text
classes_           = spaceListA "class"              :: Attribute [Text]

cols_            = intA "cols"                       :: Attribute Int
colspan_         = intA "colspan"                    :: Attribute Int
content_         = strA "content"                    :: Attribute Text
contextmenu_     = strA "contextmenu"                :: Attribute Text
controls_        = boolA "controls"                  :: Attribute Bool
coords_          = commaSep $ intA "coords"          :: Attribute [Int]
data_            = strA "data"                       :: Attribute Text
datetime_        = strA "datetime"                   :: Attribute Text
default_         = boolA "default"                   :: Attribute Bool
defer_           = boolA "defer"                     :: Attribute Bool
dir_             = strA "dir"                        :: Attribute Text
dirname_         = strA "dirname"                    :: Attribute Text
disabled_        = boolA "disabled"                  :: Attribute Bool
draggable_       = ifA "true" "false" "draggable" :: Attribute Bool
dropzone_        = spaceListA "dropzone"             :: Attribute [Text]
enctype_         = strA "enctype"                    :: Attribute Text
for_             = strA "for"                        :: Attribute Text
form_            = strA "form"                       :: Attribute Text
formaction_      = strA "formaction"                 :: Attribute Text
formnovalidate_  = boolA "formaction"                :: Attribute Bool
headers_         = spaceListA "headers"              :: Attribute [Text]
height_          = intA "height"                     :: Attribute Int
hidden_          = boolA "hidden"                    :: Attribute Bool
high_            = floatA "high"                     :: Attribute Float
href_            = strA "href"                       :: Attribute Text
hreflang_        = strA "hreflang"                   :: Attribute Text
http_equiv       = strA "http-equiv"                 :: Attribute Text
icon_            = strA "icon"                       :: Attribute Text
id_              = strA "id"                         :: Attribute Text
ismap_           = boolA "ismap"                     :: Attribute Bool
itemprop_        = strA "itemprop"                   :: Attribute Text
keytype_         = strA "keytype"                    :: Attribute Text
kind_            = strA "kind"                       :: Attribute Text
label_           = strA "label"                      :: Attribute Text
lang_            = strA "lang"                       :: Attribute Text
list_            = strA "list"                       :: Attribute Text
loop_            = boolA "loop"                      :: Attribute Bool
low_             = floatA "low"                      :: Attribute Float
manifest_        = strA "manifest"                   :: Attribute Text
max_             = floatA "max"                      :: Attribute Float
maxlength_       = intA "maxlength"                  :: Attribute Int
media_           = strA "media"                      :: Attribute Text
method_          = strA "method"                     :: Attribute Text
min_             = floatA "min"                      :: Attribute Float
multiple_        = boolA "multiple"                  :: Attribute Bool
name_            = strA "name"                       :: Attribute Text
novalidate_      = boolA "novalidate"                :: Attribute Bool
open_            = boolA "open"                      :: Attribute Bool
optimum_         = floatA "optimum"                  :: Attribute Float
pattern_         = strA "pattern"                    :: Attribute Text
ping_            = strA "ping"                       :: Attribute Text
placeholder_     = strA "placeholder"                :: Attribute Text
poster_          = strA "poster"                     :: Attribute Text
preload_         = strA "preload"                    :: Attribute Text
radiogroup_      = strA "radiogroup"                 :: Attribute Text
readonly_        = boolA "readonly"                  :: Attribute Bool
rel_             = spaceListA "rel"                  :: Attribute [Text]
required_        = boolA "required"                  :: Attribute Bool
reversed_        = boolA "reversed"                  :: Attribute Bool
rows_            = intA "rows"                       :: Attribute Int
role_            = strA "role"                       :: Attribute Text
rowspan_         = intA "rowspan"                    :: Attribute Int
sandbox_         = spaceListA "sandbox"              :: Attribute [Text]
scope_           = strA "scope"                      :: Attribute Text
spellcheck_      = ifA "true" "false" "scope"     :: Attribute Bool
scoped_          = boolA "scoped"                    :: Attribute Bool
seamless_        = boolA "seamless"                  :: Attribute Bool
selected_        = boolA "selected"                  :: Attribute Bool
shape_           = strA "shape"                      :: Attribute Text
size_            = intA "size"                       :: Attribute Int
sizes_           = spaceListA "sizes"                :: Attribute [Text]
span_            = intA "span"                       :: Attribute Int
src_             = strA "src"                        :: Attribute Text
srcdoc_          = strA "srcdoc"                     :: Attribute Text
srclang_         = strA "srclang"                    :: Attribute Text
start_           = intA "start"                      :: Attribute Int
step_            = floatA "step"                     :: Attribute Float
style_           = styleA "style"                    :: Attribute [(Text, Text)]
summary_         = strA "summary"                    :: Attribute Text
tabindex_        = intA "tabindex"                   :: Attribute Int
target_          = strA "target"                     :: Attribute Text
title_           = strA "title"                      :: Attribute Text
type_            = strA "type"                       :: Attribute Text
usemap_          = strA "usemap"                     :: Attribute Text
value_           = strA "value"                      :: Attribute Text
version_         = strA "version"                    :: Attribute Text
width_           = intA "width"                      :: Attribute Int
wrap_            = strA "wrap"                       :: Attribute Text


classList :: Reflex t => [Dynamic t Text] -> Property t
classList classes = classes_ ~: distributeListOverDynPure classes


