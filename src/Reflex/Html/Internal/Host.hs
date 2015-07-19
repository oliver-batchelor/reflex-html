module Reflex.Html.Internal.Host 
  ( module Reflex.Host.App
  , module Reflex
  , module Reflex.Host.Class
  , module Data.Dependent.Sum
  
  , module Reflex.Html.Internal.Host 
--   , collect, collection
--   , dynamicView, indexedView
--   
--   , schedulePostBuild_
  
  ) where

import Reflex
import Reflex.Host.Class

import Reflex.Host.App
import Reflex.Host.App.Internal

import Reflex.Html.Internal.Util

import Data.Maybe

import Data.Monoid

import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup.Applicative
import Data.Dependent.Sum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Foldable as F

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Fix

  
type Actions t = HostFrame t (AppInfo t) 
  
  
schedulePostBuild_ ::  (MonadAppHost t m) => HostFrame t () -> m ()
schedulePostBuild_ action = performPostBuild_ $ action >> pure mempty
  
  
mergeList' :: Reflex t => [Event t a]  -> Event t [a]
mergeList' = (fmap NE.toList) . mergeList
  
  
-- Could use a set, but probably not necessary as will be used with tiny lists? 
filterIndexes :: [Int] -> [a] -> [a] 
filterIndexes inds as = map snd $ filter (not . flip elem inds . fst) $ zip [(0::Int)..] as
 
  
collect ::  (MonadHold t m, Reflex t, MonadFix m) => Event t [(a, Event t ())] -> m (Event t [a]) 
collect newItems = do
  rec
    itemDyn <- foldDyn ($) mempty $ mergeWith (.)
      [ filterIndexes <$> removeIds
      , mappend <$> newItems 
      ]
    let removeEvents = zipWith (<$) (enumFrom (0::Int)) . fmap snd <$> current itemDyn
        removeIds    = switch (mergeList' <$> removeEvents) 
                
  pure $ fmap fst <$> updated itemDyn  
  

switchActions :: (MonadAppHost t m, Functor f, Foldable f) => Event t (f (HostFrame t (AppInfo t))) -> m ()
switchActions info = void $ performAppHost $ performPostBuild_ <$> getApp . foldMap id . fmap Ap <$> info

  
hostCollection :: MonadAppHost t m => Event t [m (a, Event t ())] -> m (Event t [a])
hostCollection newItems = do
    runAppHost <- getRunAppHost
    newViews <- performEvent $ mapM runAppHost <$> newItems
    (info, results) <- splitE . fmap unzip <$> collect (fmap rearrange <$> newViews) 
    results <$ switchActions info    
      where
        rearrange (a, (b, c)) = ((a, b), c)  
 
  
dynamicView :: (MonadAppHost t m) => Event t a -> (Dynamic t a -> m (Event t b)) -> m (Event t b) 
dynamicView e view = do
  (first, rest) <- headTailE e  
  r <- performAppHost $ ffor first $ \v -> do
    holdDyn v rest >>= view
  switchPromptly never r 
  
  

-- data Diff a = Added a | Removed a
-- 
--   
-- diffMapHost :: (MonadAppHost t m, Ord k) => Event t (Map k (m a)) -> m (Dynamic t (Map k a), Event t (Map k (Diff a)))
-- diffMapHost input  = do
--   runAppHost <- getRunAppHost  
--   rec
--     viewsDyn <- foldDyn ($) mempty $ mergeWith (.) $ 
--       [ mappend       <$> newViews 
--       , flip (Map.\\) <$> removedViews
--       ]
--       
--     removedViews <- performEvent $ return <$> diffWith (Map.\\)  (current viewsDyn) input
--     newViews     <- performEvent $ mapM runAppHost <$> diffWith (flip (Map.\\)) (current viewsDyn) input
--     
--   switchActions $ fmap fst <$> updated viewsDyn  
--   valuesDyn <- mapDyn (fmap snd) viewsDyn
--    
--   return $ (valuesDyn, appendEvents (fmap (Added . snd) <$> newViews) (fmap (Removed . snd) <$> removedViews))
--     where 
--       diffWith f curr next = ffilter (not . Map.null) $ attachWith f curr next
--   
-- 
-- newKeys :: (Reflex t, Ord k) => Dynamic t (Map k a) -> Event t (Map k a)
-- newKeys d = ffilter (not . Map.null) $ 
--   attachWith (flip Map.difference) (current d) (updated d)  
  
  {-
hostIndexedView ::  (MonadAppHost t m, Ord k) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
hostIndexedView itemsDyn itemView = do  
  items <- dynamicEvents itemsDyn
  updatedViews <- hostCollection $ (fmap runItem . Map.toList <$> newKeys itemsDyn)
  holdDyn mempty (Map.fromList <$> updatedViews)
  
  where
    runItem (k, v) = do 
      valueDyn <- holdDyn v $ fmapMaybe id updatedValue
      r <- itemView k valueDyn
      pure ((k, r), removed)
        where
          updatedValue = Map.lookup k <$> updated itemsDyn
          removed = void $ ffilter isNothing updatedValue-}

  
  
  
