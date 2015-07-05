{-# LANGUAGE  FunctionalDependencies #-}

module Reflex.Dom.Html.Internal.Element where

import Reflex
import Reflex.Dom hiding (Attributes, buildEmptyElement, buildElement)


import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Element  as Dom
import qualified GHCJS.DOM.Node  as Dom

import Data.Proxy

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.Trans

import Reflex.Dom.Html.Internal.Attributes
import Reflex.Dom.Html.Internal.Events
import Reflex.Dom.Html.Internal.Html
import Reflex.Dom.Html.Internal.Tag

import Unsafe.Coerce


type Tag = String

data Element t = Element 
  { _element_element :: Dom.Element
  ,  _element_events  :: Events t
  }  
  

class HasReflex r where
  type T r :: * 
  
instance HasReflex (Element t) where
  type T (Element t) = t 
  
  
class HasReflex e => IsElement e  where
  toElement ::  e -> Element (T e)
  
instance IsElement (Element t) where
  toElement = id
  


addAttribute :: (MonadWidget t m, Dom.IsElement e) => e -> (Key, ValueA t m) -> m ()
addAttribute dom (k, StaticA mStr) = liftIO $ forM_ mStr $ Dom.elementSetAttribute dom k
addAttribute dom (k, DynamicA makeDyn) = makeDyn >>= \d -> do
  
  schedulePostBuild $ do 
    initial <- sample (current d) 
    forM_ initial (liftIO . Dom.elementSetAttribute dom k)
     
  performEvent_ $ addRemove <$> updated d
  
  where
    addRemove Nothing    = liftIO $ Dom.elementRemoveAttribute dom k
    addRemove (Just new) = liftIO $ Dom.elementSetAttribute dom k new

    
    
buildEmptyElement :: (MonadWidget t m) => String -> String -> Attributes t m -> m Dom.Element
buildEmptyElement namespace elementTag attrs = do
  doc <- askDocument
  p <- askParent
  
  Just dom <- liftIO $ Dom.documentCreateElementNS doc namespace elementTag
  
  mapM_ (addAttribute dom) (flattenA attrs)
  _ <- liftIO $ Dom.nodeAppendChild p $ Just dom
  return $ Dom.castToElement dom  
  

  
 
      
domElement :: IsElement e => e -> Dom.Element
domElement = _element_element . toElement 


buildElement :: (MonadWidget t m) => Tag -> Attributes t m -> Html c m a -> Html p m (Dom.Element, a)
buildElement tag attrs child = do
  dom <- lift $ buildEmptyElement htmlNamespace tag attrs
  r <- subWidget (Dom.toNode $ dom) (unsafeCoerce child)    
  return (dom, r)


element' :: (MonadWidget t m) =>  Tag -> Attributes t m -> Html c m a -> Html p m (Element t, a)
element' tag attrs child = do
  (dom, r) <- buildElement tag attrs child  
  events <- bindEvents dom
  return (Element dom events, r)

element_ :: (MonadWidget t m) =>  Tag -> Attributes t m ->  Html c m a -> Html p m a
element_ tag attrs child = snd <$> buildElement tag attrs child  


  
htmlNamespace :: String
htmlNamespace = "http://www.w3.org/1999/xhtml"


-- svgElement' :: (MonadWidget t m) =>  String -> Attributes t m -> Html c m a -> Html p m (Element e t, a)
-- svgElement' = element' (Just svgNamespace)  
--   
-- svgNamespace :: String   
-- svgNamespace = "http://www.w3.org/2000/svg" 
--   
-- svgElement_ :: (MonadWidget t m) => String -> Attributes t m -> Html c m a -> Html p m a
-- svgElement_  = element_ (Just svgNamespace)   


  
-- lift an event binding from one which works on a Dom.Element to one working on an Element
liftEvent :: IsElement e => ([EventFlag] -> Dom.Element ->  m (Event (T e) a)) ->  
              [EventFlag] -> e ->  m (Event (T e) a)
liftEvent binding flags e = binding flags (domElement e) 