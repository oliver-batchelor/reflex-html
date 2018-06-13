module Builder.Html.Attributes where

import Prelude

import Builder.Attribute
import Builder.TH.Attribute

import Data.Text (Text)
import Data.GADT.Compare.TH
-- 
-- data HtmlAttribute a where
--   Hidden :: HtmlAttribute Bool
--   Class  :: HtmlAttribute [Text]
--   Style  :: HtmlAttribute [(Text, Text)]
--   Custom :: Text -> HtmlAttribute Text



$(mkAttrs "HtmlAttribute" 
  [ commaSep $ strA "accept"               
  , spaceSep $ strA "accept-charset"       
  , strA "accesskey"                  
  , strA "align"                      
  , strA "action"                     
  , strA "alt"                        
  , boolA "async"                     
  , ifA "autocomplete" ("on", "off")
  , boolA "autofocus"                 
  , boolA "autoplay"                  
  , boolA "autosave"                  
  , ifA  "border" ("1", "")
  , strA "challenge"                  
  , strA "charset"                    
  , boolA "checked"                   
  , strA "cite"                       
  , spaceSep $ strA "class"              
  , intA "cols"                       
  , intA "colspan"                    
  , strA "content"                    
  , strA "contextmenu"                
  , boolA "controls"                  
  , commaSep $ intA "coords"          
  , strA "data"                       
  , strA "datetime"                   
  , boolA "default"                   
  , boolA "defer"                     
  , strA "dir"                        
  , strA "dirname"                    
  , boolA "disabled"                  
  , ifA "draggable" ("true", "false")
  , spaceSep $ strA "dropzone"             
  , strA "enctype"                    
  , strA "for"                        
  , strA "form"                       
  , strA "formaction"    
  , strA "formenctype"                              
  , spaceSep $ strA "headers"              
  , intA "height"                     
  , boolA "hidden"                    
  , floatA "high"                     
  , strA "href"                       
  , strA "hreflang"                   
  , strA "http-equiv"                 
  , strA "icon"                       
  , strA "id"                         
  , boolA "ismap"                     
  , strA "itemprop"                   
  , strA "keytype"                    
  , strA "kind"                       
  , strA "label"                      
  , strA "lang"                       
  , strA "list"                       
  , boolA "loop"                      
  , floatA "low"                      
  , strA "manifest"                   
  , floatA "max"                      
  , intA "maxlength"                  
  , strA "media"                      
  , strA "method"                     
  , floatA "min"                      
  , boolA "multiple"                  
  , strA "name"                       
  , boolA "novalidate"                
  , boolA "open"                      
  , floatA "optimum"                  
  , strA "pattern"                    
  , strA "ping"                       
  , strA "placeholder"                
  , strA "poster"                     
  , strA "preload"                    
  , strA "radiogroup"                 
  , boolA "readonly"                  
  , spaceSep $ strA "rel"                  
  , boolA "required"                  
  , boolA "reversed"                  
  , intA "rows"                       
  , strA "role"                       
  , intA "rowspan"                    
  , spaceSep $ strA "sandbox"              
  , ifA "scope" ("true", "false")
  , boolA "scoped"                    
  , boolA "seamless"                  
  , boolA "selected"                  
  , strA "shape"                      
  , intA "size"                       
  , spaceSep $ strA "sizes"                
  , intA "span"                       
  , strA "src"                        
  , strA "srcdoc"                     
  , strA "srclang"                    
  , intA "start"                      
  , floatA "step"                     
  , styleA "style"                    
  , strA "summary"                    
  , intA "tabindex"                   
  , strA "target"                     
  , strA "title"                      
  , strA "type"                       
  , strA "usemap"                     
  , strA "value"                      
  , strA "version"                    
  , intA "width"                      
  , strA "wrap"                       
   
  ])

deriveGEq ''HtmlAttribute
deriveGCompare ''HtmlAttribute

