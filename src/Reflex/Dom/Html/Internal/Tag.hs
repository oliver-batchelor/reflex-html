{-# LANGUAGE  UndecidableInstances, TemplateHaskell #-}
module Reflex.Dom.Html.Internal.Tag where

import Data.Singletons.TH


$(singletons [d| 
  
-- | The datatype for types of tags
  data Tag  
-- | Document root
    =  Html_
-- | Document meta            
    | Base_
    | Head_
    | Link_
    | Meta_
    | Title_
    | Style_

-- | Scripting
    | Script_
    | NoScript_

-- | Content sectioning
    | Body_
    | Section_
    | Nav_
    | Article_
    | Aside_
    | H1_
    | H2_
    | H3_
    | H4_
    | H5_
    | H6_
    | HGroup_
    | Header_
    | Footer_
    | Address_

-- | Grouping content
    | P_
    | Hr_
    | Br_
    | Pre_
    | BlockQuote_
    | Ol_
    | Ul_
    | Li_
    | Dl_
    | Dt_
    | Dd_
    | Figure_
    | FigCaption_
    | Div_

-- | Text level semantics
    | A_
    | Em_
    | Strong_
    | Small_
    | S_
    | Cite_
    | Q_
    | Dfn_
    | Abbr_
    | Time_
    | Code_
    | Var_
    | Samp_
    | Kbd_
    | Sub_
    | Sup_
    | I_
    | B_
    | U_
    | Mark_
    | Ruby_
    | Rt_
    | Rp_
    | Bdi_
    | Bdo_
    | Span_

-- | Edits
    | Ins_
    | Del_

-- | Embedded content
    | Img_
    | IFrame_
    | Embed_
    | Object_
    | Param_
    | Video_
    | Audio_
    | Source_
    | Track_
    | Canvas_
    | Map_
    | Area_

-- | Tables
    | Table_
    | Caption_
    | ColGroup_
    | Col_
    | TBody_
    | THead_
    | TFoot_
    | Tr_
    | Td_
    | Th_

-- | Forms
    | Form_
    | FieldSet_
    | Legend_
    | Label_
    | Input_
    | Button_
    | Select_
    | DataList_
    | OptGroup_
    | Option_
    | TextArea_
    | KeyGen_
    | Output_
    | Progress_
    | Meter_

-- | Interactive elements
    | Details_
    | Summary_
    | Command_
    | Menu_
        deriving (Eq, Ord, Show)
        
  data ContentModel 
    = Flow 
    | Phrasing 
    | Meta
    | TextOnly 
    | None
      deriving (Eq, Ord, Show)
          
  |])
  

  
tagName :: Tag -> String
tagName Base_     = "base"
tagName Head_     = "head"
tagName Link_     = "link"
tagName Meta_     = "meta"
tagName Title_    = "title"
tagName Style_    = "style"

-- | Scripting
tagName Script_   = "script"
tagName NoScript_ = "noscript"

-- | Content sectioning
tagName Body_     = "body"
tagName Section_  = "section"
tagName Nav_      = "nav"
tagName Aside_    = "aside"
tagName H1_       = "h1"
tagName H2_       = "h2"
tagName H3_       = "h3"
tagName H4_       = "h4"
tagName H5_       = "h5"
tagName H6_       = "h6"
tagName HGroup_   = "hgroup"
tagName Header_   = "header"
tagName Footer_   = "footer"
tagName Address_  = "address"

-- | Grouping content
tagName P_        = "p"
tagName Hr_       = "hr"
tagName Br_       = "br"
tagName Pre_      = "pre"
tagName BlockQuote_ = "blockquote"
tagName Ol_       = "ol"
tagName Ul_       = "ul"
tagName Li_       = "li"
tagName Dl_       = "dl"
tagName Dt_       = "dt"
tagName Dd_       = "dd"
tagName Figure_   = "figure"
tagName FigCaption_ = "figcaption"
tagName Div_      = "div"

-- | Text level semantics
tagName A_        = "a"
tagName Em_       = "em"
tagName Strong_   = "strong"
tagName Small_    = "small"
tagName S_        = "s"
tagName Cite_     = "cite"
tagName Q_        = "q"
tagName Dfn_      = "dfn"
tagName Abbr_     = "abbr"
tagName Time_     = "time"
tagName Code_     = "code"
tagName Var_      = "var"
tagName Samp_     = "samp"
tagName Kbd_      = "kbd"
tagName Sub_      = "sub"
tagName Sup_      = "sup"
tagName I_        = "i"
tagName B_        = "b"
tagName U_        = "u"
tagName Mark_     = "mark"
tagName Ruby_     = "ruby"
tagName Rt_       = "rt"
tagName Rp_       = "rp"
tagName Bdi_      = "bdi"
tagName Bdo_      = "bdo"
tagName Span_     = "span"

-- | Edits
tagName Ins_      = "ins"
tagName Del_      = "del"

-- | Embedded content
tagName Img_      = "img"
tagName IFrame_   = "iframe"
tagName Embed_    = "embed"
tagName Object_   = "object"
tagName Param_    = "param"
tagName Video_    = "video"
tagName Audio_    = "audio"
tagName Source_   = "source"
tagName Track_    = "track"
tagName Canvas_   = "canvas"
tagName Map_      = "map"
tagName Area_     = "area"

-- | Tables
tagName Table_    = "table"
tagName Caption_  = "caption"
tagName ColGroup_ = "colgroup"
tagName Col_      = "col"
tagName TBody_    = "tbody"
tagName THead_    = "thead"
tagName TFoot_    = "tfoot"
tagName Tr_       = "tr"
tagName Td_       = "td"
tagName Th_       = "th"

-- | Forms
tagName Form_     = "form"
tagName FieldSet_ = "fieldset"
tagName Legend_   = "legend"
tagName Label_    = "label"
tagName Input_    = "input"
tagName Button_   = "button"
tagName Select_   = "select"
tagName DataList_ = "datalist"
tagName OptGroup_ = "optgroup"
tagName Option_   = "option"
tagName TextArea_ = "textarea"
tagName KeyGen_   = "keygen"
tagName Output_   = "output"
tagName Progress_ = "progress"
tagName Meter_    = "meter"

-- | Interactive elements
tagName Details_  = "details"
tagName Summary_  = "summary"
tagName Command_  = "command"
tagName Menu_     = "menu"  



