{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction, InstanceSigs #-}

module Reflex.Dom.Html.Internal.Element where

import Reflex
import Reflex.Dom hiding (Attributes)

import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Element  as Dom
import qualified GHCJS.DOM.Node  as Dom

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

import Reflex.Dom.Html.Internal.Attributes
import Reflex.Dom.Html.Internal.Events


data Element t = Element 
  { _element_element :: Dom.Element
  , _element_keypress :: Event t KeyCode
  , _element_keydown  :: Event t KeyCode
  , _element_keyup    :: Event t KeyCode
  , _element_scrolled :: Event t Int
  , _element_clicked  :: Event t ()  
  }  
  
class IsElement element  where
  toElement ::  element t -> Element t


instance IsElement Element where
  toElement = id
  
  

addAttribute :: (MonadWidget t m, Dom.IsElement e) => e -> (Key, ValueA t m) -> m ()
addAttribute e (k, StaticA mStr) = liftIO $ forM_ mStr $ Dom.elementSetAttribute e k
addAttribute e (k, DynamicA makeDyn) = makeDyn >>= \d -> do
  
  schedulePostBuild $ do 
    initial <- sample (current d) 
    forM_ initial (liftIO . Dom.elementSetAttribute e k)
     
  performEvent_ $ addRemove <$> updated d
  
  where
    addRemove Nothing    = liftIO $ Dom.elementRemoveAttribute e k
    addRemove (Just new) = liftIO $ Dom.elementSetAttribute e k new

    
    
buildEmptyElementNS :: (MonadWidget t m) => Maybe String -> String -> Attributes t m -> m Dom.Element
buildEmptyElementNS namespace elementTag attrs = do
  doc <- askDocument
  p <- askParent
  
  Just e <- liftIO $ case namespace of 
    Just ns -> Dom.documentCreateElementNS doc ns elementTag
    Nothing -> Dom.documentCreateElement doc elementTag
  
  mapM_ (addAttribute e) (flattenA attrs)
  _ <- liftIO $ Dom.nodeAppendChild p $ Just e
  return $ Dom.castToElement $ e  
  
  
  
makeElement :: MonadWidget t m => Dom.Element -> m (Element t)
makeElement e = Element e
      <$> keypressEvent_ [] e
      <*> keydownEvent_ [] e
      <*> keyupEvent_ [] e
      <*> scrolledEvent_ [] e
      <*> clickedEvent_ [] e  
      
domElement :: IsElement element => element t -> Dom.Element
domElement = _element_element . toElement 


element' :: (MonadWidget t m) =>  Maybe String -> String -> Attributes t m ->  m a -> m (Element t, a)
element' ns tag attrs child = do
  domElem <- buildEmptyElementNS ns tag attrs
  e <- makeElement domElem
  result <- subWidget (Dom.toNode $ domElem) child    
  return (e, result)

element_ :: (MonadWidget t m) =>  Maybe String -> String -> Attributes t m ->  m a -> m a
element_ ns tag attrs child = do
  domElem <- buildEmptyElementNS ns tag attrs
  subWidget (Dom.toNode $ domElem) child    

  
-- lift an event binding from one which works on a Dom.Element to one working on an Element
liftEvent :: IsElement element => ([EventFlag] -> Dom.Element ->  m (Event t a)) ->  
              [EventFlag] -> element t ->  m (Event t a)
liftEvent binding flags e = binding flags (domElement e)
