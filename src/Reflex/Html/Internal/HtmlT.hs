{-# LANGUAGE UndecidableInstances #-}

module Reflex.Html.Internal.HtmlT where

import Reflex.Html.Internal.Host


import qualified GHCJS.DOM as Dom
import qualified GHCJS.DOM.Types as Dom


import Control.Monad.Reader
import Control.Monad.Exception


data Env = Env { _env_document :: Dom.Document
               , _env_parent   :: Dom.Node }
               

newtype HtmlT m a = HtmlT { unHtmlT :: ReaderT Env m a } deriving (Functor, Applicative, Monad, 
   MonadTrans, MonadFix, MonadAsyncException, MonadException, MonadIO)
 

runHtmlT :: Dom.Document -> Dom.Node -> HtmlT m a -> m a
runHtmlT doc root html = runReaderT (unHtmlT html) (Env doc root)
  


deriving instance MonadHold t m => MonadHold t (HtmlT m) 
deriving instance MonadSample t m => MonadSample t (HtmlT m) 



askDocument :: MonadAppHost t m => HtmlT m Dom.Document
askDocument = HtmlT $ reader _env_document

askParent :: MonadAppHost t m => HtmlT m Dom.Node
askParent = HtmlT $ reader _env_parent

localChild :: MonadAppHost t m => Dom.Node -> HtmlT m a -> HtmlT m a
localChild node  = HtmlT . local reParent . unHtmlT
  where reParent env = env {_env_parent = node}

