{-# LANGUAGE   TemplateHaskell #-}

module Reflex.Html.Input 
  (  def, (&), (.~)
  , InputElement, InputConfig
  , HasValue (..), HasSetValue (..)
  , HasFocus (..), HasSetFocus (..)
  
  , textInput
  , textArea
  
  , checkboxView
  , checkboxInput
  
  , selectInput
  
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Lens
import Data.Maybe
import Data.List
import Data.Default

import qualified Data.Map as Map
import Data.Map (Map)
import Safe

import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.HTMLElement as Dom
import qualified GHCJS.DOM.HTMLInputElement as Dom
import qualified GHCJS.DOM.HTMLTextAreaElement as Dom
import qualified GHCJS.DOM.HTMLSelectElement as Dom


import Reflex.Html.Internal.Element
import Reflex.Html.Internal.Events
import Reflex.Html.Internal.Attributes
import Reflex.Html.Internal.HtmlT
import Reflex.Html.Internal.Host

import Reflex.Html.Events
import Reflex.Html.Attributes
import Reflex.Html.Elements

  
-- Value of an element
class HasReflex e => HasValue e  where
  type Value e :: *
  value   :: e -> Dynamic (T e) (Value e)
  changed :: e -> Event (T e) (Value e)   -- Change events from user input (not programatically changed)

  
class HasReflex c => HasSetValue c where
  type SetValue c :: *  
  setValue      :: Lens' c (Event (T c) (SetValue c))  
  initialValue  :: Lens' c (SetValue c)

class HasReflex e => HasFocus e   where
  hasFocus :: e -> Dynamic (T e) Bool
  
class HasReflex c => HasSetFocus c where
  setFocus :: Lens' c (Event (T c) Bool)    
  
 
data InputElement t a = InputElement 
    { _input_value :: Dynamic t a
    , _input_changed :: Event t a
    , _input_hasFocus :: Dynamic t Bool
    , _input_element :: Element t 
    }
               
data InputConfig t a = InputConfig 
    { _inputConfig_initialValue :: a
    , _inputConfig_setValue :: Event t a
    , _inputConfig_setFocus :: Event t Bool
    }          
  
                      
liftM concat $ mapM makeLenses
  [ ''InputConfig
  , ''InputElement
  , ''Element
  ]      
  

-- InputElement instances
instance Reflex t => HasReflex (InputElement t a) where
  type T (InputElement t a) = t
  
instance Reflex t =>  HasReflex (InputConfig t a) where
  type T (InputConfig t a) = t
  
               
instance Reflex t => HasValue (InputElement t a) where
  type Value (InputElement t a) = a
  value = _input_value
  changed = _input_changed
                 
                 
                 
instance Reflex t => IsElement (InputElement t a) where
  toElement = _input_element
  
instance Reflex t => HasFocus (InputElement t a) where
  hasFocus = _input_hasFocus  
                      
instance (Reflex t, Default a) => Default (InputConfig t a) where
  def = InputConfig { _inputConfig_initialValue = def
                        , _inputConfig_setValue = never
                        , _inputConfig_setFocus = never
                        }                      
                        
instance Reflex t => HasSetValue (InputConfig t a) where
  type SetValue (InputConfig t a) = a
  setValue     = inputConfig_setValue
  initialValue = inputConfig_initialValue 
  
instance Reflex t => HasSetFocus (InputConfig t a) where
  setFocus = inputConfig_setFocus 
  

  
               
inputElement :: MonadAppHost t m => String -> Attributes t m -> HtmlT m (Element t)
inputElement inputType attrs  = fst <$> input' attrs' (return ())
  where attrs' = overrideA (type_ -: inputType) $  attrs
        

setFocus_ :: MonadAppHost t m => Element t -> Event t Bool -> HtmlT m ()
setFocus_ e eSetFocus = lift $ performEvent_ $ ffor eSetFocus $ \focus -> liftIO $ if focus 
      then Dom.elementFocus (domElement e)
      else Dom.elementBlur (domElement e)        
        




makeInput_ :: (MonadAppHost t m, Dom.IsElement dom) => (Dom.Element -> dom) -> (dom -> a -> IO ()) -> (dom -> IO a) 
              -> InputConfig t a -> Element t ->  HtmlT m (InputElement t a)
makeInput_ cast setter getter (InputConfig initial eSetValue eSetFocus) e = do
  liftIO $ setter dom initial
  lift $ performEvent_ $ liftIO . setter dom  <$> eSetValue
  eChanged <- lift $ performEvent $ liftIO (getter dom) <$ domEvent Input e
  
  setFocus_ e eSetFocus 
  dFocus <- holdFocus e
  dValue <- holdDyn initial $ leftmost [eSetValue, eChanged]
  return $ InputElement dValue eChanged dFocus e 
  where
    dom = cast $ domElement e


textInput :: MonadAppHost t m => Attributes t m -> InputConfig t String -> HtmlT m (InputElement t String)
textInput attrs config  =  do
  e <- inputElement "text" attrs 
  makeInput_ Dom.castToHTMLInputElement Dom.htmlInputElementSetValue Dom.htmlInputElementGetValue config e
  
             
textArea :: MonadAppHost t m => Attributes t m -> InputConfig t String -> HtmlT m (InputElement t String)
textArea attrs config = textarea' attrs (return ()) >>= \(e, _) ->  do
  makeInput_ Dom.castToHTMLTextAreaElement Dom.htmlTextAreaElementSetValue Dom.htmlTextAreaElementGetValue config e        
             
             
-- Checkbox
checkboxView :: MonadAppHost t m => Attributes t m -> Dynamic t Bool -> HtmlT m (Event t Bool)
checkboxView attrs dValue = do
  e <- inputElement "checkbox" attrs 
  let dom = Dom.castToHTMLInputElement $ domElement e

  lift $ schedulePostBuild_ $ do
    v <- sample $ current dValue
    when v $ liftIO $ Dom.htmlInputElementSetChecked dom True
    
  --TODO: Need to preventDefault to avoid a loop
  click <- lift $ performEvent $ liftIO (Dom.htmlInputElementGetChecked dom) <$ clicked e
  lift $ performEvent_ $ fmap (\v -> liftIO $ Dom.htmlInputElementSetChecked dom $! v) $ updated dValue
  return click          
  

  
checkboxInput :: MonadAppHost t m => Attributes t m -> InputConfig t Bool -> HtmlT m (InputElement t Bool)
checkboxInput attrs config  = do
  e <- inputElement "checkbox" attrs 
  makeInput_ Dom.castToHTMLInputElement Dom.htmlInputElementSetChecked Dom.htmlInputElementGetChecked config e
  
  
selectInput :: (MonadAppHost t m) => Attributes t m -> InputConfig t String -> HtmlT m a -> HtmlT m (InputElement t String, a)
selectInput attrs  config child = do
  (e, a) <- select' attrs $ child
  input <- makeInput_ Dom.castToHTMLSelectElement Dom.htmlSelectElementSetValue Dom.htmlSelectElementGetValue config e
  return (input, a)
 
  
