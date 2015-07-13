module Reflex.Html.Internal.Host 
  ( module Reflex.Host.App
  , module Reflex
  , module Reflex.Host.Class
  , module Data.Dependent.Sum
  
  , collect, collection
  , dynamicView, indexedView
  
  , schedulePostBuild_
  
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


-- -- | Run a HostFrame action in the post build and fire it's event
-- -- | in the next frame. Typical use is sampling from Dynamics/Behaviors
-- -- | and providing the result in an Event more convenient to use.
-- performPostBuild ::  (MonadAppHost t m) => HostFrame t a -> m (Event t a)
-- performPostBuild action = do
--   (event, construct) <- newEventWithConstructor  
--   performPostBuild_ $ do
--     a <- action
--     pure $ infoFire $ liftIO (F.foldMap pure <$> construct a)
--   return event  
-- 
-- -- | Provide an event which is triggered in the next frame.
-- getPostBuild :: (MonadAppHost t m) =>  m (Event t ())
-- getPostBuild  = performPostBuild (return ())  
-- 
-- 
-- -- | Run some IO asynchronously in another thread and return it as an Event
-- performEventAsync :: MonadAppHost t m => Event t (IO a) -> m (Event t a)
-- performEventAsync event = do
--   (result, fire) <- newExternalEvent
--   performEvent_ $ void . liftIO . forkIO .  (void . fire =<<) <$> event
--   return result 
  
  
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
  

collection :: MonadAppHost t m => Event t [m (a, Event t ())] -> m (Event t [a])
collection newItems = do
    runAppHost <- getRunAppHost
    newViews <- performEvent $ mapM runAppHost <$> newItems
    (info, results) <- splitE . (fmap unzip) <$> collect (fmap rearrange <$> newViews)        
    performAppHost $ performPostBuild_ <$>
      getApp . mconcat . fmap Ap <$> info
    
    pure results
      where
        rearrange (a, (b, c)) = ((a, b), c)  
  
  


  
dynamicView :: (MonadAppHost t m) => Event t a -> (Dynamic t a -> m b) -> m (Event t b) 
dynamicView e view = do
  (first, rest) <- headTailE e  
  performAppHost $ ffor first $ \v -> do
    holdDyn v rest >>= view
  
  
indexedView ::  (MonadAppHost t m, Ord k) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
indexedView itemsDyn itemView = do  
  items <- dynamicEvents itemsDyn
  updatedViews <- collection $ (fmap runItem . Map.toList) <$> items
  holdDyn mempty (Map.fromList <$> updatedViews)
  
  where
    newItems = attachWith (flip Map.difference) (current itemsDyn) (updated itemsDyn)
    runItem (k, v) = do 
      valueDyn <- holdDyn v $ fmapMaybe id updatedValue
      r <- itemView k valueDyn
      pure ((k, r), removed)
        where
          updatedValue = Map.lookup k <$> updated itemsDyn
          removed = () <$ ffilter isNothing updatedValue

  
  

  
