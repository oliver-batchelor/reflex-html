{-# LANGUAGE  FunctionalDependencies #-}

module Reflex.Html.Internal.Element where


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

import Reflex.Html.Internal.Attributes
import Reflex.Html.Internal.Events
import Reflex.Html.Internal.HtmlT
import Reflex.Html.Internal.Host


type Tag = String
type Namespace = String

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
  


addAttribute :: (MonadAppHost t m, Dom.IsElement e) => e -> (Key, ValueA t m) -> m ()
addAttribute dom (k, StaticA mStr) = liftIO $ forM_ mStr $ Dom.elementSetAttribute dom k
addAttribute dom (k, DynamicA makeDyn) = makeDyn >>= \d -> do
  
  schedulePostBuild_ $ do 
    initial <- sample (current d) 
    forM_ initial (liftIO . Dom.elementSetAttribute dom k)
     
  performEvent_ $ addRemove <$> updated d
  
  where
    addRemove Nothing    = liftIO $ Dom.elementRemoveAttribute dom k
    addRemove (Just new) = liftIO $ Dom.elementSetAttribute dom k new

    
    
buildEmptyElement :: MonadAppHost t m => String -> String -> Attributes t m -> HtmlT m Dom.Element
buildEmptyElement namespace elementTag attrs = do
  doc <- askDocument
  p <- askParent

  Just dom <- liftIO $ Dom.documentCreateElementNS doc namespace elementTag  
  lift $ mapM_ (addAttribute dom) (flattenA attrs)
  void $ liftIO $ Dom.nodeAppendChild p $ Just dom

  return $ Dom.castToElement dom  
  
 
      
domElement :: IsElement e => e -> Dom.Element
domElement = _element_element . toElement 


 
buildElement :: MonadAppHost t m => Namespace -> Tag -> Attributes t m -> HtmlT m a -> HtmlT m (Dom.Element, a)
buildElement namespace tag attrs child = do
  dom <- buildEmptyElement namespace tag attrs
  r <- localChild (Dom.toNode dom) child
  return (dom, r)


element' :: MonadAppHost t m =>  Namespace -> Tag -> Attributes t m -> HtmlT m a -> HtmlT m (Element t, a)
element' ns tag attrs child = do
  (dom, r) <- buildElement ns tag attrs child  
  events <- lift $ bindEvents dom
  return (Element dom events, r)

element_ :: MonadAppHost t m => Namespace -> Tag -> Attributes t m ->  HtmlT m a -> HtmlT m a
element_ ns tag attrs child = snd <$> buildElement ns tag attrs child  


  
htmlNamespace :: String
htmlNamespace = "http://www.w3.org/1999/xhtml"


htmlElement' :: MonadAppHost t m =>  Tag -> Attributes t m -> HtmlT m a -> HtmlT m (Element t, a)
htmlElement' = element' htmlNamespace

htmlElement_ :: MonadAppHost t m =>  Tag -> Attributes t m -> HtmlT m a -> HtmlT m a
htmlElement_  = element_ htmlNamespace


svgNamespace :: String   
svgNamespace = "http://www.w3.org/2000/svg" 

svgElement' :: MonadAppHost t m =>  Tag -> Attributes t m -> HtmlT m a -> HtmlT m (Element t, a)
svgElement'  = element' svgNamespace

svgElement_ :: MonadAppHost t m =>  Tag -> Attributes t m -> HtmlT m a -> HtmlT m a
svgElement_  = element_ svgNamespace


  
-- lift an event binding from one which works on a Dom.Element to one working on an Element
liftEvent :: IsElement e => ([EventFlag] -> Dom.Element ->  m (Event (T e) a)) ->  
              [EventFlag] -> e ->  m (Event (T e) a)
liftEvent binding flags e = binding flags (domElement e) 