{-# LANGUAGE UndecidableInstances, ImpredicativeTypes #-}

module Reflex.Html.Internal.HtmlT where

import Reflex.Spider
import Reflex.Html.Internal.Host
import Reflex.Html.Internal.WebView
import Reflex.Host.App.Internal


import qualified GHCJS.DOM as Dom
import qualified GHCJS.DOM.Node as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.HTMLElement as Dom
import qualified GHCJS.DOM.Types as Dom

import Control.Monad.Reader
import Control.Monad.Exception
import Control.Concurrent
import Control.Monad.Trans.Maybe



data Env = Env { _env_document :: Dom.Document
               , _env_parent   :: Dom.Node }
               

newtype HtmlT m a = HtmlT { unHtmlT :: ReaderT Env m a } deriving (Functor, Applicative, Monad, MonadHold t, MonadSample t,
            MonadReflexCreateTrigger t, MonadFix, MonadTrans, MonadIO)
   

runHtmlT :: MonadAppHost t m => Dom.Document -> Dom.Node -> HtmlT m a -> m a
runHtmlT doc root html = runReaderT (unHtmlT html) (Env doc root)

askRunHtmlT :: (MonadAppHost t m) =>  HtmlT m (HtmlT m a -> m a)
askRunHtmlT = do
  (Env doc parent) <- HtmlT $ ask
  pure $ runHtmlT doc parent
  

askDocument :: MonadAppHost t m => HtmlT m Dom.Document
askDocument = HtmlT $ reader _env_document

askParent :: MonadAppHost t m => HtmlT m Dom.Node
askParent = HtmlT $ reader _env_parent

localChild :: MonadAppHost t m => Dom.Node -> HtmlT m a -> HtmlT m a
localChild node  = HtmlT . local reParent . unHtmlT
  where reParent env = env {_env_parent = node}

  

  

attachRoot :: MonadAppHost t m => Dom.Document -> Dom.HTMLElement -> HtmlT m () -> m ()
attachRoot doc root child = do  
  liftIO $ Dom.htmlElementSetInnerHTML root ""
  void $ runHtmlT doc (Dom.toNode root) $ do
    (_, df) <- runInFragment child
    liftIO $ Dom.nodeAppendChild (Dom.toNode root) $ Just df
    
    
 
      
runHtml :: HtmlT (AppHost Spider) () -> HtmlT (AppHost Spider) () -> IO ()
runHtml head body =  liftIO $ runWebGUI $ \webView -> do
  Just doc <- Dom.webViewGetDomDocument webView
  Just headElem <- Dom.documentGetHead doc
  Just bodyElem <- Dom.documentGetBody doc
  
  appState <- runSpiderHost $ initHostApp $ do
    attachRoot doc (Dom.toHTMLElement headElem) head
    attachRoot doc (Dom.toHTMLElement bodyElem) body
  
  forM_ appState $ \(chan, step) -> void . liftIO . forkIO . forever $ do 
    liftIO (readChan chan) >>= Dom.postGUISync . runSpiderHost . step 
 
 
runInFragment :: MonadAppHost t m => HtmlT m a -> HtmlT m (a, Dom.DocumentFragment)
runInFragment child = do
  doc <- askDocument
  
  Just df <- liftIO $ Dom.documentCreateDocumentFragment doc
  r <- localChild (Dom.toNode df) $ child
  return (r, df)
  


  
  

  
    
    