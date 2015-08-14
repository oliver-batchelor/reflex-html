{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Reflex.Html.Internal.Collection where


import qualified GHCJS.DOM as Dom
import qualified GHCJS.DOM.Node as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Types as Dom

import Reflex.Html.Internal.Element
import Reflex.Html.Internal.Attributes

import Reflex.Html.Internal.HtmlT
import Reflex.Html.Internal.Host
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.Traversable
import Data.Foldable


import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Tuple


data Diff a = Added a | Removed deriving (Show, Functor)

$(makePrisms ''Diff)

-- -- May be different parent than it was at initial construction, e.g., because the parent may have dumped us in from a DocumentFragment
deleteBetweenExclusive :: (Dom.Node, Dom.Node) -> IO ()
deleteBetweenExclusive (start, end) = traverse_ removeFrom =<< Dom.nodeGetParentNode end  where
  removeFrom parent = do 
    node <- Dom.nodeGetPreviousSibling end
    eq <- Dom.nodeIsEqualNode start node
    when eq $ do
      Dom.nodeRemoveChild parent node >> removeFrom parent

        
-- | s and e must both be children of the same node and s must precede e
deleteBetweenInclusive :: (Dom.Node, Dom.Node) -> IO ()
deleteBetweenInclusive (start, end) = do
  mayParent <- Dom.nodeGetParentNode end
  for_ mayParent $ \parent -> do
    removeFrom parent
    Dom.nodeRemoveChild parent (Just end)

  where   
    removeFrom parent = do 
      node <- Dom.nodeGetPreviousSibling end
      Dom.nodeRemoveChild parent node
      eq <- Dom.nodeIsEqualNode start node
      when eq $ removeFrom parent
  
  
  

type NodeRange = (Dom.Node, Dom.Node)


wrapRange :: Dom.Document -> Dom.DocumentFragment -> IO NodeRange
wrapRange doc fragment = do
  Just start <- fmap Dom.toNode <$> Dom.documentCreateTextNode doc ""
  Just end   <- fmap Dom.toNode <$> Dom.documentCreateTextNode doc ""

  firstChild <- Dom.nodeGetFirstChild node
  Dom.nodeInsertBefore node (Just start) firstChild
  Dom.nodeAppendChild node (Just end)
  return (start, end)
    where
      node = Dom.toNode fragment
      

holdDomList :: (MonadAppHost t m, Ord k) => Event t (Map k (Diff (Dom.DocumentFragment, a))) -> HtmlT m (Dynamic t (Map k a))
holdDomList changes = do
  endMarker <- Dom.toNode <$> text' ""
  updateList <- modifyDomList <$> askDocument <*> askParent <*> pure endMarker  

  rec
    viewsDyn <- holdDyn mempty updatedViews
    updatedViews <- lift $ performEvent $ liftIO <$> attachWith (flip updateList) (current viewsDyn) changes 
      
  mapDyn (fmap snd) viewsDyn  
   
    
modifyDomList :: forall k a. Ord k => Dom.Document -> Dom.Node -> Dom.Node -> Map k (Diff (Dom.DocumentFragment, a)) -> Map k (NodeRange, a) -> IO (Map k (NodeRange, a))
modifyDomList doc parent endNode fragments =  execStateT (imapM_ modifyM fragments)  where
  modifyM k item = get >>= liftIO . modifyItem k item >>= put      
  
  modifyItem ::  k -> Diff (Dom.DocumentFragment, a) -> Map k (NodeRange, a) -> IO (Map k (NodeRange, a))
  modifyItem k (Added (frag, a)) items =  do
    range <- wrapRange doc frag
    Dom.nodeInsertBefore parent (Just frag) (Just endMarker) 
    return (Map.insert k (range, a) items)
    where
      endMarker = fromMaybe endNode (fst . fst . snd <$> Map.lookupGT k items) 
  modifyItem k Removed items = do 
    mapM_ deleteBetweenInclusive (fst <$> Map.lookup k items)
    return $ Map.delete k items
      
      
    
diffKeys :: (Ord k) => Map k a -> Map k b -> Map k (Diff b)
diffKeys m m' = (Added <$> m' Map.\\ m)  <> (const Removed <$>  m Map.\\ m')  


diffInput :: (Reflex t, Ord k) => Behavior t (Map k a) -> Event t (Map k b) -> Event t (Map k (Diff b))
diffInput currentItems updatedItems = ffilter (not . Map.null) $ 
  attachWith diffKeys currentItems updatedItems


domList :: (MonadAppHost t m, Ord k, Show k) => Event t (Map k (HtmlT m a)) -> HtmlT m (Dynamic t (Map k a))
domList input  = do
  runAppHost <- lift getRunAppHost  
  runHtmlT <- askRunHtmlT
  rec
    let runView = fmap rearrange . runAppHost . runHtmlT . runInFragment
        rearrange (info, (a, frag)) = (frag, (info, a))
        changes = diffInput  (current viewsDyn) input 
        
--     lift $ performEvent $ liftIO . print . fmap (fmap (const ())) <$> changes

    changedViews <- lift . performEvent $ mapMOf (traverse . _Added) runView <$> changes
    viewsDyn <- holdDomList changedViews
    
  lift $ switchActions (fmap fst <$> (updated viewsDyn))
  mapDyn (fmap snd) viewsDyn

  
makeView :: (MonadAppHost t m, Ord k, Show k) =>  Dynamic t (Map k v) -> (k -> Dynamic t v -> HtmlT m a) -> HtmlT m (Event t (Map k (HtmlT m a)))
makeView d view = fmap (Map.mapWithKey itemView) <$> lift (dynToEvents d)
  where  itemView k v = holdDyn v (fmapMaybe (Map.lookup k) (updated d)) >>= view k


  
listWithKey :: (MonadAppHost t m, Ord k, Show k) => Dynamic t (Map k v) -> (k -> Dynamic t v -> HtmlT m a) -> HtmlT m (Dynamic t (Map k a))
listWithKey d view = makeView d view >>= domList 

list :: (MonadAppHost t m, Ord k, Show k) => Dynamic t (Map k v) -> (Dynamic t v -> HtmlT m a) -> HtmlT m (Dynamic t (Map k a))
list d view = listWithKey  d (const view)



  