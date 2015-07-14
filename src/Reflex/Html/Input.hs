{-# LANGUAGE   TemplateHaskell #-}

module Reflex.Html.Input 
  (  def, (&), (.~)
  , Input, InputConfig
  , HasValue (..), HasSetValue (..)
  , HasFocus (..), HasSetFocus (..)
  
--   , textInput
--   , textArea
--   
--   , checkboxView
--   , checkboxInput
--   
--   , selectInput
  
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
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
  
 
data Input t a = Input { _input_value :: Dynamic t a
               , _input_changed :: Event t a
               , _input_hasFocus :: Dynamic t Bool
               , _input_element :: Element t 
               }
               
data InputConfig t a
    = InputConfig { _inputConfig_initialValue :: a
                  , _inputConfig_setValue :: Event t a
                  , _inputConfig_setFocus :: Event t Bool
                  }          
  
                      
liftM concat $ mapM makeLenses
  [ ''InputConfig
  , ''Input
  , ''Element
  ]      
  

-- Input instances
instance HasReflex (Input t a) where
  type T (Input t a) = t
  
instance HasReflex (InputConfig t a) where
  type T (InputConfig t a) = t
  
               
instance HasValue (Input t a) where
  type Value (Input t a) = a
  value = _input_value
  changed = _input_changed
                 
                 
                 
instance IsElement (Input t a) where
  toElement = _input_element
  
instance HasFocus (Input t a) where
  hasFocus = _input_hasFocus  
                      
instance (Reflex t, Default a) => Default (InputConfig t a) where
  def = InputConfig { _inputConfig_initialValue = def
                        , _inputConfig_setValue = never
                        , _inputConfig_setFocus = never
                        }                      
                        
instance HasSetValue (InputConfig t a) where
  type SetValue (InputConfig t a) = a
  setValue     = inputConfig_setValue
  initialValue = inputConfig_initialValue 
  
instance HasSetFocus (InputConfig t a) where
  setFocus = inputConfig_setFocus 
  

  
{-
               
inputElement :: MonadWidget t m => String -> Attributes t m -> HtmlT p m (Element Input_ t)
inputElement inputType attrs  = fst <$> input' attrs' (return ())
  where attrs' = overrideA (type_ -: inputType) $  attrs
        

setFocus_ :: MonadWidget t m => Element t -> Event t Bool -> m ()
setFocus_ e eSetFocus = performEvent_ $ ffor eSetFocus $ \focus -> liftIO $ if focus 
      then Dom.elementFocus (domElement e)
      else Dom.elementBlur (domElement e)        
        
        
inputEvent_ :: (MonadWidget t m, Dom.IsElement e) =>  e -> (e -> IO a) -> m (Event t a)
inputEvent_ e f = wrapEvent Dom.elementOninput (liftIO $ f e) [] e 


makeInput_ :: (MonadWidget t m, Dom.IsElement e) => (Dom.Element -> e) -> (e -> a -> IO ()) -> (e -> IO a) -> InputConfig t a -> Element tag t ->  m (Input tag t a)
makeInput_ cast setter getter (InputConfig initial eSetValue eSetFocus) e = do
  liftIO $ setter dom initial
  performEvent_ $ liftIO . setter dom  <$> eSetValue
  eChanged <- inputEvent_ dom getter
  setFocus_ e eSetFocus 
  dFocus <- holdFocus e
  dValue <- holdDyn initial $ leftmost [eSetValue, eChanged]
  return $ Input dValue eChanged dFocus e 
  where
    dom = cast $ domElement e


textInput :: MonadWidget t m => Attributes t m -> InputConfig String t -> HtmlT p m (Input String t)
textInput attrs config  =  do
  e <- inputElement "text" attrs 
  makeInput_ Dom.castToHTMLInputElement Dom.htmlInputElementSetValue Dom.htmlInputElementGetValue config e
  
             
textArea :: MonadWidget t m => Attributes t m -> InputConfig String t -> HtmlT p m (Input String t)
textArea attrs config = textArea' attrs (return ()) >>= \(e, _) ->  do
  makeInput_ Dom.castToHTMLTextAreaElement Dom.htmlTextAreaElementSetValue Dom.htmlTextAreaElementGetValue config e        
             
             
-- Checkbox

checkboxView :: MonadWidget t m => Attributes t m -> Dynamic t Bool -> HtmlT p m (Event t Bool)
checkboxView attrs dValue = do
  e <- inputElement "checkbox" attrs 
  let dom = Dom.castToHTMLInputElement $ domElement e
  eClicked <- wrapEvent Dom.elementOnclick (liftIO $ Dom.htmlInputElementGetChecked dom) [PreventDefault] (domElement e)  
  schedulePostBuild $ do
    v <- sample $ current dValue
    when v $ liftIO $ Dom.htmlInputElementSetChecked dom True
  performEvent_ $ fmap (\v -> liftIO $ Dom.htmlInputElementSetChecked dom $! v) $ updated dValue
  return eClicked          
  

  
checkboxInput :: MonadWidget t m => Attributes t m -> InputConfig Bool t -> HtmlT p m (Input Bool t)
checkboxInput attrs config  = do
  e <- inputElement "checkbox" attrs 
  makeInput_ Dom.castToHTMLInputElement Dom.htmlInputElementSetChecked Dom.htmlInputElementGetChecked config e
  
  
selectInput :: (MonadWidget t m) => Attributes t m -> InputConfig String t -> HtmlT c m a -> HtmlT p m (Input String t, a)
selectInput attrs  config child = do
  (e, a) <- select' attrs $ child
  input <- makeInput_ Dom.castToHTMLSelectElement Dom.htmlSelectElementSetValue Dom.htmlSelectElementGetValue config e
  return (input, a)
 
  
  -}