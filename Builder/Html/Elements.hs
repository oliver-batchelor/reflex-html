module Builder.Html.Elements where

import Prelude

import Builder.Element
import Builder.TH

import Data.Text

elem ::  Text -> Elem
elem elemName props child = snd <$> makeElem' Nothing elemName props child
{-# INLINE elem #-}

elem_ ::  Text -> Elem_
elem_ elemName props child = fst <$> makeElem' Nothing elemName props child
{-# INLINE elem_ #-}

child_ ::  Text -> Child_
child_ elemName props = fst <$> makeElem' Nothing elemName props (return ())
{-# INLINE child_ #-}

elem' :: Text -> Elem'
elem' = makeElem' Nothing
{-# INLINE elem' #-}


$(mkElems Nothing

  --Document metadata
  [ C "link"
  , C "meta"
  , E "style"
  , E "title"
  , C "base"

  --Content sectioning
  , E "address"
  , E "article"
  , E "aside"
  , E "footer"
  , E "header"
  , E "h1", E "h2", E "h3", E "h4", E "h5", E "h6"
  , E "hgroup"
  , E "nav"
  , E "section"

  -- Text content
  , E "blockquote"
  , E "dd"
  , E "dir"
  , E "div"
  , E "dl"
  , E "dt"
  , E "figcaption"
  , E "figure"
  , C "hr"
  , E "li"
  , E "main"
  , E "ol"
  , E "p"
  , E "pre"
  , E "ul"

  -- Inline text semantics
  , E "a"
  , E "abbr"
  , E "b"
  , E "bdi"
  , E "bdo"
  , C "br"
  , E "cite"
  , E "code"
  -- , E "data"
  , E "dfn"
  , E "em"
  , E "i"
  , E "kbd"
  , E "mark"
  , E "q"
  , E "rp"
  , E "rt"
  , E "rtc"
  , E "ruby"
  , E "s"
  , E "samp"
  , E "small"
  , E "span"
  , E "strong"
  , E "sub"
  , E "sup"
  , E "time"
  , E "tt"
  , E "u"
  , E "var"
  , C "wbr"

  -- Image and multimedia
  , C "area"
  , E "audio"
  , C "img"
  , E "map"
  , C "track"
  , E "video"

  -- Embedded content
  , E "applet"
  , C "embed"
  , E "iframe"
  , E "noembed"
  , E "object"
  , C "param"
  , E "picture"
  , C "source"

  --Scripting
  , C "canvas"
  , E "noscript"
  , E "script"

  -- Demarcating edits
  , E "del"
  , E "ins"

  --Table content
  , E "caption"
  , C "col"
  , E "colgroup"
  , E "table"
  , E "tbody"
  , E "td"
  , E "tfoot"
  , E "th"
  , E "thead"
  , E "tr"

  -- Forms
  , E "button"
  , E "datalist"
  , E "fieldset"
  , E "form"
  , C "input"
  , E "label"
  , E "legend"
  , E "meter"
  , E "optgroup"
  , E "option"
  , E "output"
  , E "progress"
  , E "select"
  , E "textarea"

  -- Interactive elements
  , E "details"
  , E "dialog"
  , E "menu"
  , C "menuitem"
  , E "summary"

  -- Web Components
  , E "content"
  , E "element"
  , E "shadow"
  , E "slot"
  , E "template"
  ])
