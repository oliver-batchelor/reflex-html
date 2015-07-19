module Reflex.Html.Time 
  ( delay  
  
  ) where


import Reflex.Html.Internal.Host
import Reflex.Html.Internal.HtmlT

import Data.Time.Clock
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad


 


delay :: MonadAppHost t m => NominalDiffTime -> Event t a -> HtmlT m (Event t a)
delay dt e = lift $ performEventAsync $ ffor e $ \a -> do
  liftIO $ threadDelay $ ceiling $ dt * 1000000
  return a