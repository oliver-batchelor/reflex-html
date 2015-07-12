{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction, TupleSections, TemplateHaskell, StandaloneDeriving, GeneralizedNewtypeDeriving, UndecidableInstances, EmptyDataDecls #-}

module Reflex.Dom.Html.Internal.Html where

import Reflex.Dom
import Reflex.Dom.Class

import Reflex.Host.Class

import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Exception

import Reflex.Dom.Html.Internal.Tag

data Env = Env

newtype Html (cm :: ContentModel) m a = Html { unHtml :: ReaderT Env m a } deriving (Functor, Applicative, Monad, 
  HasDocument, HasWebView, MonadTrans, MonadFix, MonadAsyncException, MonadException, MonadIO)
 
runHtml_ :: Html cm m a -> Env -> m a
runHtml_ html env = runReaderT (unHtml html) env

runHtml :: Html cm m a -> m a
runHtml html = runHtml_ html Env
  

instance MonadRef m => MonadRef (Html cm m) where
  type Ref (Html cm m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (Html cm m)
deriving instance HasPostGui t h m => HasPostGui t h (Html cm m) 
deriving instance MonadHold t m => MonadHold t (Html cm m) 
deriving instance MonadSample t m => MonadSample t (Html cm m) 

askEnv :: Monad m => Html cm m Env
askEnv = Html $ ask  

instance MonadWidget t m => MonadWidget t (Html cm m) where
  type WidgetHost (Html cm m) = WidgetHost m
  type GuiAction (Html cm m) = GuiAction m
  askParent = lift askParent
  
  subWidget n w = do
    r <- askEnv
    lift $ subWidget n $ runHtml_ w r
  subWidgetWithVoidActions n w = do
    r <- askEnv
    lift $ subWidgetWithVoidActions n $ runHtml_ w r
  liftWidgetHost = lift . liftWidgetHost
  schedulePostBuild = lift . schedulePostBuild
  addVoidAction = lift . addVoidAction
  getRunWidget = do
    r <- askEnv
    runWidget <- lift getRunWidget
    return $ \rootElement w -> do
      (a, postBuild, voidActions) <- runWidget rootElement $ runHtml_ w r
      return (a, postBuild, voidActions)
      
      
      