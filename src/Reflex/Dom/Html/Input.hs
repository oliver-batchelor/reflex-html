{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction, TupleSections, TemplateHaskell #-}

module Reflex.Dom.Html.Input 
  (  def, (&), (.~)
  , Input, InputConfig
  , HasValue (..), HasSetValue (..)
  , HasFocus (..), HasSetFocus (..)
  
  , textInput
  , textArea
  
  , checkboxView
  , checkbox
  
  , selection
  
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





import Reflex.Dom (performEvent_, MonadWidget, schedulePostBuild, listWithKey, dynText, (=:), wrapDomEvent)

import Reflex
import Reflex.Dom.Html.Internal.Element
import Reflex.Dom.Html.Internal.Events
import Reflex.Dom.Html.Internal.Attributes


import Reflex.Dom.Html.Events
import Reflex.Dom.Html.Attributes
import Reflex.Dom.Html.Elements


  
-- Value of an element
class HasValue e  where
  type Value e :: *
  value   :: e t -> Dynamic t (Value e)
  changed :: e t -> Event t (Value e)   -- Change events from user input (not programatically changed)

  
class HasSetValue c where
  type SetValue c :: *  
  setValue      :: Lens' (c t) (Event t (SetValue c))  
  initialValue  :: Lens' (c t) (SetValue c)

class HasFocus e   where
  hasFocus :: e t -> Dynamic t Bool
  
class HasSetFocus c where
  setFocus :: Lens' (c t) (Event t Bool)    
  
 
data Input a t = Input { _input_value :: Dynamic t a
               , _input_changed :: Event t a
               , _input_hasFocus :: Dynamic t Bool
               , _input_element :: Element t 
               }
               
data InputConfig a t
    = InputConfig { _inputConfig_initialValue :: a
                  , _inputConfig_setValue :: Event t a
                  , _inputConfig_setFocus :: Event t Bool
                  }          
  
                      
liftM concat $ mapM makeLenses
  [ ''InputConfig
  , ''Input
  , ''Element
  ]      
  

-- TextInput
               
instance HasValue (Input a) where
  type Value (Input a) = a
  value = _input_value
  changed = _input_changed
                  
instance IsElement (Input a) where
  toElement = _input_element
  
instance HasFocus (Input a) where
  hasFocus = _input_hasFocus  
                      
instance (Reflex t, Default a) => Default (InputConfig a t) where
  def = InputConfig { _inputConfig_initialValue = def
                        , _inputConfig_setValue = never
                        , _inputConfig_setFocus = never
                        }                      
                        
instance HasSetValue (InputConfig a) where
  type SetValue (InputConfig a) = a
  setValue     = inputConfig_setValue
  initialValue = inputConfig_initialValue 
  
instance HasSetFocus (InputConfig a) where
  setFocus = inputConfig_setFocus 
  
               
inputElement :: MonadWidget t m => String -> [Attribute t m] -> m (Element t)
inputElement inputType attrs  = fst <$> input' attrs' (return ())
  where attrs' = (type_ -: inputType) : attrs
        

setFocus_ :: MonadWidget t m => Element t -> Event t Bool -> m ()
setFocus_ e eSetFocus = performEvent_ $ ffor eSetFocus $ \focus -> liftIO $ if focus 
      then Dom.elementFocus (domElement e)
      else Dom.elementBlur (domElement e)        
        
        
inputEvent_ :: (MonadWidget t m, Dom.IsElement e) =>  e -> (e -> IO a) -> m (Event t a)
inputEvent_ e f = wrapEvent Dom.elementOninput (liftIO $ f e) [] e 


makeInput_ :: (MonadWidget t m, Dom.IsElement e) => (Dom.Element -> e) -> (e -> a -> IO ()) -> (e -> IO a) -> InputConfig a t -> Element t ->  m (Input a t)
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


textInput :: MonadWidget t m => [Attribute t m] -> InputConfig String t -> m (Input String t)
textInput attrs config  =  do
  e <- inputElement "text" attrs 
  makeInput_ Dom.castToHTMLInputElement Dom.htmlInputElementSetValue Dom.htmlInputElementGetValue config e
  
             
textArea :: MonadWidget t m => [Attribute t m] -> InputConfig String t -> m (Input String t)
textArea attrs config = textArea' attrs (return ()) >>= \(e, _) ->  do
  makeInput_ Dom.castToHTMLTextAreaElement Dom.htmlTextAreaElementSetValue Dom.htmlTextAreaElementGetValue config e        
             
             
-- Checkbox

checkboxView :: MonadWidget t m => [Attribute t m] -> Dynamic t Bool -> m (Event t Bool)
checkboxView attrs dValue = do
  e <- inputElement "checkbox" attrs 
  let dom = Dom.castToHTMLInputElement $ domElement e
  eClicked <- wrapEvent Dom.elementOnclick (liftIO $ Dom.htmlInputElementGetChecked dom) [PreventDefault] (domElement e)  
  schedulePostBuild $ do
    v <- sample $ current dValue
    when v $ liftIO $ Dom.htmlInputElementSetChecked dom True
  performEvent_ $ fmap (\v -> liftIO $ Dom.htmlInputElementSetChecked dom $! v) $ updated dValue
  return eClicked          
  

  
checkbox :: MonadWidget t m => [Attribute t m] -> InputConfig Bool t -> m (Input Bool t)
checkbox attrs config  = do
  e <- inputElement "checkbox" attrs 
  makeInput_ Dom.castToHTMLInputElement Dom.htmlInputElementSetChecked Dom.htmlInputElementGetChecked config e
  
  
selection :: forall k t m. (MonadWidget t m, Ord k, Show k, Read k) => [Attribute t m] -> Dynamic t (Map k String) -> InputConfig k t -> m (Input k t)
selection attrs options (InputConfig initial eSetValue eSetFocus) = do
  (e, _) <- select' attrs $ do
    optionsWithDefault <- mapDyn (`Map.union` (initial =: "")) options
    listWithKey optionsWithDefault $ \k v -> do
      option_ [value_ -: show k, selected_ -: (k == initial)] $ dynText v
 
  let dom = Dom.castToHTMLSelectElement $ domElement e

  performEvent_ $ fmap (liftIO . Dom.htmlSelectElementSetValue dom . show) eSetValue
  eChange <- wrapDomEvent dom Dom.elementOnchange $ do
    kStr <- liftIO $ Dom.htmlSelectElementGetValue dom
    return $ readMay kStr
    
  setFocus_ e eSetFocus 
  dFocus <- holdFocus e
        
  dValue <- combineDyn readKey options =<< holdDyn (Just initial) (leftmost [eChange, Just <$> eSetValue])
  return $ Input dValue (attachWith readKey (current options) eChange) dFocus e
    where 
      readKey opts mk = fromMaybe initial $ do
        k <- mk
        guard $ Map.member k opts
        return k
  