module Reflex.Html.Internal.Util where

import Control.Concurrent

import Reflex
import Reflex.Host.App

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



sampleInitial :: (MonadAppHost t m) => Dynamic t a ->  m (Event t a)
sampleInitial d  = performPostBuild $ sample (current d)


dynamicEvents ::  (MonadAppHost t m) => Dynamic t a ->  m (Event t a)
dynamicEvents d = do
  initial <- sampleInitial d
  pure $ leftmost [initial, updated d]