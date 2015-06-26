{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction, InstanceSigs #-}

module Reflex.Dom.Html.Internal where

import Reflex
import Reflex.Dom 

import Reflex.Host.Class

import GHCJS.DOM.Types hiding (Widget, Event, Element, IsElement, toElement)
import qualified GHCJS.DOM.Types as Dom

import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.DOMWindow as Dom
import qualified GHCJS.DOM.Element  as Dom
import qualified GHCJS.DOM.UIEvent  as Dom
import qualified GHCJS.DOM.EventM  as Dom 
import qualified GHCJS.DOM.Node  as Dom

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Data.Bifunctor (second)
import Data.List 


import qualified Data.Map as Map
import Data.Functor.Contravariant

import Data.IORef
import Data.Maybe

import Data.Functor.Contravariant

type KeyCode = Int



type Key = String
data EventFlag = StopPropagation | PreventDefault 

data ValueA t = StaticA (Maybe String) |  DynamicA (Dynamic t (Maybe String))

-- If Dynamic becomes a Functor we can remove the "m"
type Attribute t m = (Key, m (ValueA t))
data Attr a = Attr { 
  _attr_key :: Key, 
  _attr_map :: a -> Maybe String 
}

instance Contravariant Attr where
  contramap f (Attr k m) = Attr k (m . f)


stringAttr :: Key -> Attr String
stringAttr key = Attr key Just

showAttr :: Show a => Key -> Attr a
showAttr key = Attr key (Just . show)

boolAttr :: Key -> Attr Bool
boolAttr key = Attr key bool where
  bool True  = Just ""
  bool False = Nothing

makeGroups :: Ord k => [(k, v)] -> [(k, [v])]
makeGroups  = Map.toList . Map.fromListWith (++) . map (second pure)

  
concatValues :: (MonadHold t m, Reflex t) =>  [m (ValueA t)] -> m (ValueA t)
concatValues g = do 
  (v:vs) <- sequence g
  foldM catValues v vs 
  
-- Make attributes concatenable, if a key is specified twice then the attribute values are concatenated
catValues :: (MonadHold t m, Reflex t) =>  ValueA t -> ValueA t -> m (ValueA t)
catValues (StaticA v) (StaticA v') = return $ StaticA $ catValues' v v'
catValues (DynamicA d) (StaticA v) = DynamicA <$> mapDyn (catValues' v) d 
catValues (DynamicA d) (DynamicA d') = DynamicA <$> combineDyn catValues' d d' 
catValues v1 v2 = catValues v2 v1
  
catValues' :: Maybe String -> Maybe String -> Maybe String
catValues' v v' =  case catMaybes [v, v'] of
  [] -> Nothing
  l  -> Just $ intercalate " " l
  
    
  
instance MonadWidget t m => Attributes m [Attribute t m] where
  addAttributes attrs e = mapM_ (addAttribute e) attrs 
  
addAttribute :: (MonadWidget t m, Dom.IsElement e) => e -> Attribute t m -> m ()
addAttribute e (k, v) = v >>= add
    
  where    
    add (StaticA mStr) = liftIO $ forM_ mStr $ Dom.elementSetAttribute e k
    add (DynamicA d)  = do
      schedulePostBuild $ do 
        initial <- sample (current d) 
        forM_ initial (liftIO . Dom.elementSetAttribute e k)
        
      performEvent_ $ addRemove <$> updated d
      
    addRemove Nothing    = liftIO $ Dom.elementRemoveAttribute e k
    addRemove (Just new) = liftIO $ Dom.elementSetAttribute e k new
  
  
buildEmptyElementNS :: (MonadWidget t m, Attributes m attrs) => Maybe String -> String -> attrs -> m Dom.Element
buildEmptyElementNS namespace elementTag attrs = do
  doc <- askDocument
  p <- askParent
  
  Just e <- liftIO $ case namespace of 
    Just ns -> Dom.documentCreateElementNS doc ns elementTag
    Nothing -> Dom.documentCreateElement doc elementTag
  
  addAttributes attrs e
  _ <- liftIO $ Dom.nodeAppendChild p $ Just e
  return $ Dom.castToElement $ e  
  
  
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
  
--   toElement ::  Element t -> Element t
  toElement = id
  
  
makeElement :: MonadWidget t m => Dom.Element -> m (Element t)
makeElement e = Element e
      <$> keypressEvent_ [] e
      <*> keydownEvent_ [] e
      <*> keyupEvent_ [] e
      <*> scrolledEvent_ [] e
      <*> clickedEvent_ [] e  
  
element' :: (MonadWidget t m) =>  Maybe String -> String -> [Attribute t m] ->  m a -> m (Element t, a)
element' ns tag attrs child = do
  domElem <- buildEmptyElementNS ns tag attrs
  e <- makeElement domElem
  result <- subWidget (toNode $ domElem) child    
  return (e, result)

element_ :: (MonadWidget t m) =>  Maybe String -> String -> [Attribute t m] ->  m a -> m a
element_ ns tag attrs child = do
  domElem <- buildEmptyElementNS ns tag attrs
  subWidget (toNode $ domElem) child    
  
  
   
domElement :: IsElement element => element t -> Dom.Element
domElement = _element_element . toElement   
  
     
wrapEvent :: (MonadWidget t m, Dom.IsEvent event) => (Dom.Element -> Dom.EventM event Dom.Element () -> IO (IO ())) -> Dom.EventM event Dom.Element a -> [EventFlag] -> Dom.Element -> m (Event t a)
wrapEvent onEvent getResult flags e = do
  event <- wrapDomEvent e onEvent (mapM_ applyFlag flags >> getResult)  
  unless (null flags) $ performEvent_ $ ffor event $ const $ return ()
  return event
 
applyFlag :: IsEvent e => EventFlag -> Dom.EventM e t ()
applyFlag StopPropagation = Dom.stopPropagation
applyFlag PreventDefault = Dom.preventDefault
  

  
-- lift an event binding from one which works on a Dom.Element to one working on an Element
liftEvent :: IsElement element => ([EventFlag] -> Dom.Element ->  m (Event t a)) ->  
              [EventFlag] -> element t ->  m (Event t a)
liftEvent binding flags e = binding flags (domElement e)
  
--Raw event bindings
clickedEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
clickedEvent_ = wrapEvent Dom.elementOnclick (return ())

keypressEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element -> m (Event t Int) 
keypressEvent_ = wrapEvent Dom.elementOnkeypress  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)

keydownEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element  -> m (Event t Int)
keydownEvent_ = wrapEvent Dom.elementOnkeydown  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)

keyupEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element  -> m (Event t Int)
keyupEvent_ = wrapEvent Dom.elementOnkeyup  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)

scrolledEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element -> m (Event t Int)
scrolledEvent_ flags e = wrapEvent Dom.elementOnscroll (liftIO $ Dom.elementGetScrollTop e) flags e
  
blurEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
blurEvent_ = wrapEvent Dom.elementOnblur (return ())
 
focusEvent_ :: (MonadWidget t m) => [EventFlag] -> Dom.Element ->  m (Event t ())
focusEvent_ = wrapEvent Dom.elementOnfocus (return ()) 

-- Events on the Window level

askWindow :: (MonadIO m, HasDocument m) => m DOMWindow
askWindow =  do 
  (Just window) <- askDocument >>= liftIO . Dom.documentGetDefaultView 
  return window 

window_keydownEvent :: (MonadWidget t m) => m (Event t Int)
window_keydownEvent = askWindow >>= \e -> wrapDomEvent e Dom.domWindowOnkeydown  (liftIO . Dom.uiEventGetKeyCode =<< Dom.event)  
  