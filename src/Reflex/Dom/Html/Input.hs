{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction, TupleSections, TemplateHaskell #-}

module Reflex.Dom.Html.Input 
  ( module Reflex.Dom.Html.Input
  , def, (&), (.~)
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe
import Data.List
import Data.Default


import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.HTMLElement as Dom
import qualified GHCJS.DOM.HTMLInputElement as Dom



import Reflex.Dom (performEvent_, MonadWidget, schedulePostBuild)

import Reflex
import Reflex.Dom.Html.Internal
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
  
  
-- Types
data TextInput t
   = TextInput { _textInput_value :: Dynamic t String
               , _textInput_changed :: Event t String
               , _textInput_hasFocus :: Dynamic t Bool
               , _textInput_element :: Element t 
               }
data TextInputConfig t
    = TextInputConfig { _textInputConfig_initialValue :: String
                      , _textInputConfig_setValue :: Event t String
                      , _textInputConfig_setFocus :: Event t Bool
                      }  
                      
liftM concat $ mapM makeLenses
  [ ''TextInputConfig
  , ''TextInput
  , ''Element
  ]      
  

-- TextInput
               
instance HasValue TextInput where
  type Value TextInput = String
  value = _textInput_value
  changed = _textInput_changed
  
instance HasFocus TextInput where
  hasFocus = _textInput_hasFocus
                
instance IsElement TextInput where
  toElement = _textInput_element
                      
instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig { _textInputConfig_initialValue = ""
                        , _textInputConfig_setValue = never
                        , _textInputConfig_setFocus = never
                        }                      
                        
instance HasSetValue TextInputConfig where
  type SetValue TextInputConfig = String
  setValue     = textInputConfig_setValue
  initialValue = textInputConfig_initialValue 
  
instance HasSetFocus TextInputConfig where
  setFocus = textInputConfig_setFocus 
                      
inputElement :: MonadWidget t m => String -> [Attribute t m] -> m (Element t)
inputElement inputType attrs  = fst <$> input' attrs' (return ())
  where attrs' = type_ -: inputType : attrs
                      
textInput :: MonadWidget t m => [Attribute t m] -> TextInputConfig t -> m (TextInput t)
textInput attrs (TextInputConfig initial eSetValue eSetFocus) =  do
  e <- inputElement "text" attrs 
  let dom = Dom.castToHTMLInputElement $ domElement e
  liftIO $ Dom.htmlInputElementSetValue dom initial
  performEvent_ $ liftIO . Dom.htmlInputElementSetValue dom <$> eSetValue
  performEvent_ $ ffor eSetFocus $ \focus -> liftIO $ if focus 
      then Dom.elementFocus dom
      else Dom.elementBlur dom
  eChanged <- wrapEvent Dom.elementOninput (liftIO $ Dom.htmlInputElementGetValue dom) [] (domElement e)
  dFocus <- holdFocus e
  dValue <- holdDyn initial $ leftmost [eSetValue, eChanged]
  return $ TextInput dValue eChanged dFocus e 
             
             
-- Checkbox

checkboxView :: MonadWidget t m => [Attribute t m] -> Dynamic t Bool -> m (Event t Bool)
checkboxView attrs dValue = input' attrs (return ()) >>= \(e, _) -> do
  e <- inputElement "checkbox" attrs 
  let dom = Dom.castToHTMLInputElement $ domElement e
  eClicked <- wrapEvent Dom.elementOnclick (liftIO $ Dom.htmlInputElementGetChecked dom) [PreventDefault] (domElement e)  
  schedulePostBuild $ do
    v <- sample $ current dValue
    when v $ liftIO $ Dom.htmlInputElementSetChecked dom True
  performEvent_ $ fmap (\v -> liftIO $ Dom.htmlInputElementSetChecked dom $! v) $ updated dValue
  return eClicked           