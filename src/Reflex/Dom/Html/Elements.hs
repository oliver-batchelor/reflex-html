{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction #-}
module Reflex.Dom.Html.Elements 
  ( module Reflex.Dom.Html.Elements
  , Element  
  , IsElement (..)
  )
  where

import Reflex.Dom 
import Reflex.Dom.Html.Internal.Element
import Reflex.Dom.Html.Internal.Attributes

-- Element builders
            
  



htmlElement' :: (MonadWidget t m) =>  String -> [Attribute t m] -> m a -> m (Element t, a)
htmlElement' = element' Nothing 

htmlElement_ :: (MonadWidget t m) => String -> [Attribute t m] -> m a -> m a
htmlElement_  = element_ Nothing

svgElement' :: (MonadWidget t m) =>  String -> [Attribute t m] -> m a -> m (Element t, a)
svgElement' = element' (Just svgNamespace)  
  
svgNamespace :: String   
svgNamespace = "http://www.w3.org/2000/svg" 
  
svgElement_ :: (MonadWidget t m) => String -> [Attribute t m] -> m a -> m a
svgElement_  = element_ (Just svgNamespace) 


-- HTML elements which provide the Element for binding events

a_ = htmlElement_ "a" 
div_ = htmlElement_ "div"
footer_ = htmlElement_ "footer"
link_ = htmlElement_ "link"
p_ = htmlElement_ "p"
span_ = htmlElement_ "span"
strong_ = htmlElement_ "strong"
label_ = htmlElement_ "label"

section_ = htmlElement_ "section"
header_ = htmlElement_ "header"
 

h1_ = htmlElement_ "h1"
h2_ = htmlElement_ "h2"
h3_ = htmlElement_ "h3"
h4_ = htmlElement_ "h4"
h5_ = htmlElement_ "h5"

ul_ = htmlElement_ "ul"
li_ = htmlElement_ "li"

button_ = htmlElement_ "button"
input_ = htmlElement_ "input"
textArea_ = htmlElement_ "textArea"
select_ = htmlElement_ "select"
option_ = htmlElement_ "option"




a' = htmlElement' "a" 
div' = htmlElement' "div"
footer' = htmlElement' "footer"
link' = htmlElement' "link"
p' = htmlElement' "p"
span' = htmlElement' "span"
strong' = htmlElement' "strong"
label' = htmlElement' "label"

section' = htmlElement' "section"
header' = htmlElement' "header"
 

h1' = htmlElement' "h1"
h2' = htmlElement' "h2"
h3' = htmlElement' "h3"
h4' = htmlElement' "h4"
h5' = htmlElement' "h5"

ul' = htmlElement' "ul"
li' = htmlElement' "li"


button' = htmlElement' "button"
input' = htmlElement' "input"
textArea' = htmlElement' "textArea"
select' = htmlElement' "select"
option' = htmlElement' "option"



-- -- SVG elements
-- 
svg_ = svgElement_ "svg"
rect_ = svgElement_ "rect"
g_ = svgElement_ "g"  
  
  
svg' = svgElement' "svg"
rect' = svgElement' "rect"
g' = svgElement' "g"  

