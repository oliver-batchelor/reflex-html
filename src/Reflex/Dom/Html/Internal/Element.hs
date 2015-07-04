{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction, InstanceSigs #-}

module Reflex.Dom.Html.Internal.Element where

import Reflex
import Reflex.Dom hiding (Attributes, buildEmptyElement, buildElement)

import Data.Singletons.Prelude
import Data.Singletons

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


data Element (tag :: Tag) t = Element 
  { _element_element :: Dom.Element
  , _element_keypress :: Event t KeyCode
  , _element_keydown  :: Event t KeyCode
  , _element_keyup    :: Event t KeyCode
  , _element_scrolled :: Event t Int
  , _element_clicked  :: Event t ()  
  }  
  
class IsElement element  where
  toElement ::  element tag t -> Element tag t


instance IsElement Element where
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
  

  
bindElement :: MonadWidget t m => Dom.Element -> m (Element tag t)
bindElement dom = Element dom
      <$> keypressEvent_ [] dom
      <*> keydownEvent_ [] dom
      <*> keyupEvent_ [] dom
      <*> scrolledEvent_ [] dom
      <*> clickedEvent_ [] dom  
      
domElement :: IsElement element => element tag t -> Dom.Element
domElement = _element_element . toElement 


buildElement :: forall tag m t c a p. (SingI tag, MonadWidget t m) => Proxy (tag :: Tag) -> Attributes t m -> Html c m a -> Html p m (Dom.Element, a)
buildElement _ attrs child = do
  dom <- lift $ buildEmptyElement htmlNamespace (tagName tag) attrs
  r <- subWidget (Dom.toNode $ dom) (unsafeCoerce child)    
  return (dom, r)

  where
    (tag :: Tag) = fromSing (sing :: Sing tag) 

element' :: (SingI tag, MonadWidget t m) =>  Proxy (tag :: Tag) -> Attributes t m -> Html c m a -> Html p m (Element tag t, a)
element' tag attrs child = do
  (dom, r) <- buildElement tag attrs child  
  e <- bindElement dom
  return (e, r)

element_ :: (SingI tag, MonadWidget t m) =>  Proxy (tag :: Tag) -> Attributes t m ->  Html c m a -> Html p m a
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
liftEvent :: IsElement element => ([EventFlag] -> Dom.Element ->  m (Event t a)) ->  
              [EventFlag] -> element e t ->  m (Event t a)
liftEvent binding flags e = binding flags (domElement e)
