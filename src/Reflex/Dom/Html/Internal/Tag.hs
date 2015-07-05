{-# LANGUAGE  UndecidableInstances, TemplateHaskell #-}
module Reflex.Dom.Html.Internal.Tag where

import Data.Singletons.TH
import Data.Promotion.Prelude


$(singletons [d| 
  
        
  data ContentModel 
    = Flow 
    | Phrasing 
    | Option 
    | OptionGroup 
    | TextOnly 
      deriving (Eq, Ord, Show)
          
  |])

  
type IsPhrasingOrFlow p = (Elem p [Flow, Phrasing]) ~ True 
type IsOptions p = (Elem p [Option, OptionGroup]) ~ True 
type AllowsText p = (Elem p [TextOnly, Flow, Phrasing]) ~ True 
  
  


