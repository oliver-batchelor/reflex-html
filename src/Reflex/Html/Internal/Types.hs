{-# LANGUAGE  FunctionalDependencies #-}

module Reflex.Dom.Html.Internal.Element where

import Reflex
import Reflex.Dom hiding (Attributes, buildEmptyElement, buildElement)

import Data.Singletons.Prelude
import Data.Singletons

import Data.Proxy

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.Trans


-- Element

data Element t = Element 
  { _element_element :: Dom.Element
    _element_events  :: Events t
  }  
  
  
data Events t = Events {  
  , _event_keypress :: Event t KeyCode
  , _event_keydown  :: Event t KeyCode
  , _event_keyup    :: Event t KeyCode
  , _event_scrolled :: Event t Int
  , _event_clicked  :: Event t ()    
}
  

  
class IsElement e  where
  toElement ::  e -> Element (T e)


instance IsElement (Element t) where
  toElement = id
  
-- Attributes

type Key = String
data ValueA t m = StaticA { _staticValue :: (Maybe String) } |  DynamicA { _dynValue :: (m (Dynamic t (Maybe String))) }

liftM concat $ mapM makeLenses
  [ ''ValueA
  ]   

-- If Dynamic becomes a Functor we can remove the "m"
newtype Attributes t m = Attributes  { unAttr :: Map Key [ValueA t m] }


instance IsList (Attributes t m) where
  type Item (Attributes t m) = Attributes t m
    
  fromList = mconcat
  toList x = [x]

  
  
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
  
 
data Input a tag t = Input { _input_value :: Dynamic t a
               , _input_changed :: Event t a
               , _input_hasFocus :: Dynamic t Bool
               , _input_element :: Element tag t 
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