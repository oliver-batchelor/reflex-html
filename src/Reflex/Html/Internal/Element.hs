{-# LANGUAGE  FunctionalDependencies, TemplateHaskell #-}

module Reflex.Html.Internal.Element where


import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Element  as Dom
import qualified GHCJS.DOM.Node  as Dom

import Data.Proxy
import Data.Functor.Misc

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
  
  
data ElementConfig t m = ElementConfig
  { _namespace :: Namespace
  , _tagName   :: Tag        
  , _attributes :: Attributes t m 
  }

liftM concat $ mapM makeLenses
  [ ''ElementConfig
  ]   
  
class Reflex (T r) => HasReflex r where
  type T r :: * 
  
instance Reflex t => HasReflex (Element t) where
  type T (Element t) = t 

  
instance Reflex t => HasReflex (ElementConfig t m) where
  type T (ElementConfig t m) = t 
  
  
class HasReflex e => IsElement e  where
  toElement ::  e -> Element (T e)
  
instance Reflex t => IsElement (Element t) where
  toElement = id
  
  
class HasDomEvent t a where
  domEvent :: EventName en -> a -> Event t (EventResultType en)

instance Reflex t => HasDomEvent t (Element t) where
  domEvent en e = unEventResult <$> select (_element_events e) (WrapArg en)
  

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
  events <- bindEvents dom
  return (Element dom events, r)

element_ :: MonadAppHost t m => Namespace -> Tag -> Attributes t m ->  HtmlT m a -> HtmlT m a
element_ ns tag attrs child = snd <$> buildElement ns tag attrs child  


el_ :: MonadAppHost t m => ElementConfig t m -> HtmlT m ()
el_ (ElementConfig ns tag attrs)  = element_ ns tag attrs $ return ()  

el :: MonadAppHost t m => ElementConfig t m -> HtmlT m a -> HtmlT m a
el (ElementConfig ns tag attrs) child = element_ ns tag attrs child  

el' :: MonadAppHost t m => ElementConfig t m -> HtmlT m a -> HtmlT m (Element t, a)
el' (ElementConfig ns tag attrs) child = element' ns tag attrs child


text' :: MonadAppHost t m => String -> HtmlT m Dom.Text
text' str = do
  doc <- askDocument
  p <- askParent
  Just n <- liftIO $ Dom.documentCreateTextNode doc str
  _ <- liftIO $ Dom.nodeAppendChild p $ Just n
  return n

dynText :: (MonadAppHost t m) => Dynamic t String -> HtmlT m ()
dynText d = do
  n <- text' ""
  lift $ schedulePostBuild_ $ do
    str <- sample $ current d
    liftIO $ Dom.nodeSetNodeValue n str
  lift $ performEvent_ $ fmap (liftIO . Dom.nodeSetNodeValue n) $ updated d  

  
-- lift an event binding from one which works on a Dom.Element to one working on an Element
liftEvent :: IsElement e => ([EventFlag] -> Dom.Element -> HtmlT m (Event (T e) a)) ->  
              [EventFlag] -> e -> HtmlT m (Event (T e) a)
liftEvent binding flags e = binding flags (domElement e) 

