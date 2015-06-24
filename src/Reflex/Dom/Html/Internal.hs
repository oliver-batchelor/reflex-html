{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, ImpredicativeTypes, StandaloneDeriving, NoMonomorphismRestriction #-}

module Reflex.Dom.Html.Internal where

import Reflex
import Reflex.Dom 

import Reflex.Host.Class

import GHCJS.DOM.Types hiding (Widget, Event, Element)
import qualified GHCJS.DOM.Types as D

import GHCJS.DOM.Document
import GHCJS.DOM.DOMWindow
import GHCJS.DOM.Element hiding (Element)
import GHCJS.DOM.UIEvent
import GHCJS.DOM.EventM 
import GHCJS.DOM.Node

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.Ref
import Control.Monad.Fix
import Control.Monad.Trans

import Control.Monad.Exception
import Data.Functor.Contravariant

import Data.IORef
import Data.Maybe

    
newtype Element = Element { unElem :: D.Element }
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


  
instance MonadWidget t m => Attributes m [Attribute t m] where
  addAttributes attrs e = mapM_ (addAttribute e) attrs 
  
addAttribute :: (MonadWidget t m, IsElement e) => e -> Attribute t m -> m ()
addAttribute e (k, v) = v >>= add
    
  where    
    add (StaticA mStr) = liftIO $ forM_ mStr $ elementSetAttribute e k
    add (DynamicA d)  = do
      schedulePostBuild $ do 
        initial <- sample (current d) 
        forM_ initial (liftIO . elementSetAttribute e k)
        
      performEvent_ $ addRemove <$> updated d
      
    addRemove Nothing    = liftIO $ elementRemoveAttribute e k
    addRemove (Just new) = liftIO $ elementSetAttribute e k new
  
  
buildEmptyElementNS :: (MonadWidget t m, Attributes m attrs) => Maybe String -> String -> attrs -> m Element
buildEmptyElementNS namespace elementTag attrs = do
  doc <- askDocument
  p <- askParent
  
  Just e <- liftIO $ case namespace of 
    Just ns -> documentCreateElementNS doc ns elementTag
    Nothing -> documentCreateElement doc elementTag
  
  addAttributes attrs e
  _ <- liftIO $ nodeAppendChild p $ Just e
  return $ Element . castToElement $ e  
  
     
wrapEvent :: (MonadWidget t m, IsEvent e) => (D.Element -> EventM e D.Element () -> IO (IO ())) -> EventM e D.Element a -> [EventFlag] -> Element -> m (Event t a)
wrapEvent onEvent getResult flags (Element e) = do
  event <- wrapDomEvent e onEvent (mapM_ applyFlag flags >> getResult)  
  unless (null flags) $ performEvent_ $ ffor event $ const $ return ()
 
  return event

applyFlag :: IsEvent e => EventFlag ->  EventM e t ()
applyFlag StopPropagation = stopPropagation
applyFlag PreventDefault = preventDefault
  
askWindow :: (MonadIO m, HasDocument m) => m DOMWindow
askWindow =  do 
  (Just window) <- askDocument >>= liftIO . documentGetDefaultView 
  return window 
  
element' :: (MonadWidget t m) =>  Maybe String -> String -> [Attribute t m] -> (Element -> m a) -> m a
element' ns tag attrs child = do
  e <- buildEmptyElementNS ns tag attrs
  subWidget (toNode $ unElem e) (child e)  
