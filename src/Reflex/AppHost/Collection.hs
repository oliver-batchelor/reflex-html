{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex
import Reflex.Host.Class

import AppHost

import Data.These
import Data.Align
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import qualified Data.Foldable as F


import Debug.Trace

import Data.Maybe

import Control.Concurrent
import Data.Semigroup.Applicative


getPostBuild :: (MonadAppHost t m) =>  m (Event t ())
getPostBuild  = performPostBuild (return ())
  
performPostBuild ::  (MonadAppHost t m) => HostFrame t a -> m (Event t a)
performPostBuild action = do
  (event, construct) <- newEventWithConstructor  
  performPostBuild_ $ do
    a <- action
    pure $ infoFire $ liftIO (F.foldMap pure <$> construct a)
  return event  
  
performEventAsync :: MonadAppHost t m => Event t (IO a) -> m (Event t a)
performEventAsync event = do
  (result, fire) <- newExternalEvent
  performEvent_ $ void . liftIO . forkIO .  (void . fire =<<) <$> event
  return result 

  
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
  
  

sampleInitial :: (MonadAppHost t m) => Dynamic t a ->  m (Event t a)
sampleInitial d  = performPostBuild $ sample (current d)


dynamicEvents ::  (MonadAppHost t m) => Dynamic t a ->  m (Event t a)
dynamicEvents d = do
  initial <- sampleInitial d
  pure $ leftmost [initial, updated d]

  
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

delay :: (MonadAppHost t m) => Int -> Event t a -> m (Event t a)
delay ms e = performEventAsync $ (threadDelay ms >>) . pure <$> e 
  
  
iterateEvent :: MonadAppHost t m => (Event t a -> m (Event t a)) -> Event t a -> m (Event t a)
iterateEvent f e = do
  rec
    next <- f $ leftmost [e, next]
  return next     

unTag :: Reflex t => Event t a -> Event t ()
unTag = (() <$)
  
iterateEvent_ :: MonadAppHost t m => (Event t () -> m (Event t a)) -> m (Event t a)
iterateEvent_ f = do
  initial <- f =<< getPostBuild
  after   <- iterateEvent (f . unTag) initial  
  pure $ leftmost [initial, after]
  
       
tick :: MonadAppHost t m => Int -> m (Event t ())
tick ms = getPostBuild >>= iterateEvent (delay ms)  
  
  
main :: IO ()
main  = runSpiderHost $ hostApp $ do
-- 
  keys <- iterateEvent_ (performEventAsync . (getLine <$))   
--   
  views <- collection $ ffor keys $ \str -> pure $ do
    time <- tick 1000000
    seconds <- updated <$> count time

--     performEvent_ $ liftIO . print . (str <>) . show <$> seconds
    return (str,  () <$ ffilter (==3) seconds)
    
  performEvent_ $ liftIO . print <$> views  

  
