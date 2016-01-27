{-# LANGUAGE TemplateHaskell #-}
module Reflex.Html.Input where

import Reflex.Html.Html
import Reflex.Html.Render
import Reflex.Html.Element
import Reflex.Html.Prelude
import Reflex.Html.Elements.Html

import qualified Data.Map as Map
import Control.Lens


instance Default DomString where
  def = ""


data InputElement t a = InputElement
    { inputValue :: Dynamic t a
    , inputChanged :: Event t a
    , inputEvents :: Events t
    }

instance Reflex t => DomEvents t (InputElement t a) where
  domEvent en e = domEvent_ en (inputEvents e)

data InputConfig t a = InputConfig
    { _initialValue :: a
    , _setValue :: Event t a
    }

-- Generate simpleLenses (Lens') otherwise the generated lenses are more
-- polymorphic than is helpful, causing ambiguous type errors around the Reflex t
liftM concat $ mapM (makeLensesWith (lensRules & simpleLenses .~ True))
  [ ''InputConfig ]

instance (Default a, Reflex t) => Default (InputConfig t a) where
  def = InputConfig def def


textInput :: Renderer t => [Property t] -> InputConfig t DomString -> Html t (InputElement t DomString)
textInput properties (InputConfig initial setE) = do
  value <- holdDyn initial setE
  elem  <- input_ ((type_ =: "text") :  (SetTextInput ~: value) : properties) $ pure ()

  value <- holdDyn initial $ leftmost [inputText elem, setE]
  return $ InputElement value (inputText elem) (elemEvents elem)


