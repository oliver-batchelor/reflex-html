{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, NoMonomorphismRestriction #-}

module Reflex.Dom.Html.Elements  where

import Reflex.Dom 


import GHCJS.DOM.Types hiding (Event, Element)
import qualified GHCJS.DOM.Types as D

import GHCJS.DOM.Document
import GHCJS.DOM.DOMWindow
import GHCJS.DOM.Element hiding (Element)
import GHCJS.DOM.UIEvent
import GHCJS.DOM.EventM 

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe
import Data.List

import Reflex.Dom.Html.Internal


-- Attribute binders
(-:) :: MonadWidget t m => Attr a -> a -> Attribute t m
a -: v = (_attr_key a, value)
  where value = return $ StaticA (_attr_map a v)

  
(~:) :: MonadWidget t m => Attr a -> Dynamic t a -> Attribute t m
a ~: v = (_attr_key a, value)
  where value = DynamicA <$> mapDyn (_attr_map a) v 


(~?) :: MonadWidget t m => Attr a -> Dynamic t (Maybe a) -> Attribute t m
a ~? v = (_attr_key a, value)
  where value = DynamicA <$> mapDyn (>>= _attr_map a) v 


-- Element builders

htmlElement' :: (MonadWidget t m) =>  String -> [Attribute t m] -> (Element -> m a) -> m a
htmlElement' = element' Nothing 

htmlElement_ :: (MonadWidget t m) => String -> [Attribute t m] -> m a -> m a
htmlElement_ tag attrs child = htmlElement' tag attrs (const child)

svgElement' :: (MonadWidget t m) =>  String -> [Attribute t m] -> (Element -> m a) -> m a
svgElement' = element' (Just ns) where 
  ns = "http://www.w3.org/2000/svg" 

svgElement_ :: (MonadWidget t m) => String -> [Attribute t m] -> m a -> m a
svgElement_ tag attrs child = htmlElement' tag attrs (const child)



-- -- HTML attributes
-- width_, height_, class_, href_, style_ ::  Attr String
width_ = stringAttr "width"
height_ = stringAttr "height"
class_ = stringAttr "class"
href_ = stringAttr "href"
style_ = stringAttr "style"
for_ = stringAttr "for"
visibility_ = stringAttr "visibility"

rows_ :: Attr Int
rows_ = showAttr "rows"

hidden_ :: Attr Bool
hidden_ = boolAttr "hidden" 


-- Convenience combiators for attributes 
toMaybe :: a -> Bool -> Maybe a
toMaybe a True = Just a
toMaybe _ _    = Nothing

classes_ :: Attr [String]
classes_ = contramap (concat . intersperse " ") $ class_

-- HTML elements which provide the Element for binding events

a_ = htmlElement_ "a" 
div_ = htmlElement_ "div"
footer_ = htmlElement_ "footer"
link_ = htmlElement_ "link"
p_ = htmlElement_ "p"
span_ = htmlElement_ "span"
strong_ = htmlElement_ "strong"
button_ = htmlElement_ "button"
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



a' = htmlElement' "a" 
div' = htmlElement' "div"
footer' = htmlElement' "footer"
link' = htmlElement' "link"
p' = htmlElement' "p"
span' = htmlElement' "span"
strong' = htmlElement' "strong"
button' = htmlElement' "button"
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




-- --Event bindings
clickedWith :: (MonadWidget t m) => [EventFlag] -> Element ->  m (Event t ())
clickedWith = wrapEvent elementOnclick (return ())
clicked = clickedWith []

keypressWith :: (MonadWidget t m) => [EventFlag] -> Element -> m (Event t Int) 
keypressWith = wrapEvent elementOnkeypress  (liftIO . uiEventGetKeyCode =<< event)
keypress = keypressWith []


keydownWith :: (MonadWidget t m) => [EventFlag] -> Element  -> m (Event t Int)
keydownWith = wrapEvent elementOnkeydown  (liftIO . uiEventGetKeyCode =<< event)
keydown = keydownWith []


scrolledWith :: (MonadWidget t m) => [EventFlag] -> Element -> m (Event t Int)
scrolledWith flags (Element e) = wrapEvent elementOnscroll (liftIO $ elementGetScrollTop e) flags (Element e)
scrolled = scrolledWith []
  

-- Events on the Window level

window_keydown :: (MonadWidget t m) => m (Event t Int)
window_keydown = askWindow >>= \e -> wrapDomEvent e domWindowOnkeydown  (liftIO . uiEventGetKeyCode =<< event)  


-- -- SVG elements
-- 
svg_, rect_, g_ :: (MonadWidget t m) => [Attribute t m] ->  m a -> m a
svg_ = svgElement_ "svg"
rect_ = svgElement_ "rect"
g_ = svgElement_ "g"  
  
  
svg', rect', g' :: (MonadWidget t m) => [Attribute t m] -> (Element -> m a) -> m a
svg' = svgElement' "svg"
rect' = svgElement' "rect"
g' = svgElement' "g"  


