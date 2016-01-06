{-# LANGUAGE TemplateHaskell #-}
module Reflex.Html.Input where

import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.Event as Dom
import qualified GHCJS.DOM.EventM as Dom
import qualified GHCJS.DOM.HTMLElement as Dom
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.HTMLTextAreaElement as Area
import qualified GHCJS.DOM.HTMLSelectElement as Select


import Reflex.Html.Html
import Reflex.Html.Render
import Reflex.Monad

import Control.Monad.IO.Class

import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Lens

import Reflex
import Reflex.Host.Class

-- Value of an element
class HasReflex e => HasValue e  where
  type Value e :: *
  value   :: e -> Dynamic (T e) (Value e)
  changed :: e -> Event (T e) (Value e)   -- Change events from user input (not programatically changed)

class HasReflex c => HasSetValue c where
  type SetValue c :: *
  setValue      :: Lens' c (Event (T c) (SetValue c))
  initialValue  :: Lens' c (SetValue c)


class HasReflex c => HasSetFocus c where
  setFocus :: Lens' c (Event (T c) Bool)


data InputElement t a = InputElement
    { _input_value :: Dynamic t a
    , _input_changed :: Event t a
    , _input_events :: Events t
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

instance Reflex t => HasReflex (InputElement t a) where
  type T (InputElement t a) = t

instance Reflex t => HasReflex (InputConfig t a) where
  type T (InputConfig t a) = t

instance (Default a, Reflex t) => Default (InputConfig t a) where
  def = InputConfig def def def

instance Reflex t => HasSetValue (InputConfig t a) where
  type SetValue (InputConfig t a) = a
  setValue = inputConfig_setValue
  initialValue = inputConfig_initialValue

instance Reflex t => HasValue (InputElement t a) where
  type Value (InputElement t a) = a
  value = _input_value
  changed = _input_changed

instance Reflex t => HasSetFocus (InputConfig t a) where
  setFocus = inputConfig_setFocus

text :: Renderer t => String -> Html t ()
text str = build_ $  buildText str

dynText :: Renderer t => Dynamic t String -> Html t ()
dynText d = build_ $ do
  text <- sample (current d) >>= buildText
  render (updated d) $ updateText text


element :: Renderer t => String -> String -> [Attribute t] -> Html t (Events t)
element ns tag attrs = do
  dynAttrs <- holdAttributes attrs
  build $ bindEvents =<< buildElement_ ns tag dynAttrs


element_ :: Renderer t => String -> String -> [Attribute t] -> Html t a -> Html t a
element_ ns tag attrs child = do
  (a, r) <- collectBuilder child
  dynAttrs <- holdAttributes attrs
  build_ $ buildElement ns tag dynAttrs r
  return a

element' :: Renderer t => String -> String -> [Attribute t] -> Html t a -> Html t (a, Element t)
element' ns tag attrs child = do
  (a, r) <- collectBuilder child
  dynAttrs <- holdAttributes attrs
  events <- build $ bindEvents . fst =<< buildElement ns tag dynAttrs r
  return (a, Element events)

updateFocus :: (Renderer t, Dom.IsElement e) => e -> Dynamic t Bool ->  Builder t ()
updateFocus e focus = do
  initial <- sample (current focus)
  render (updated focus) $ \case
      True  -> liftIO $ Dom.focus e
      False -> liftIO $ Dom.blur e

updateDyn :: Renderer t => Dynamic t a -> (e -> Maybe a -> IO ()) -> e -> Builder t ()
updateDyn d setter e = do
  liftIO . setter e . Just =<< sample (current d)
  render (updated d) $ liftIO . setter e . Just


makeInput :: (Renderer t, Dom.IsElement e) => (e -> Maybe a -> IO ()) -> (e -> IO (Maybe a))
              -> InputConfig t a -> Builder t e -> Html t (InputElement t a)
makeInput setter getter config create = do
  f <- holdDyn False (config ^. setFocus)
  v <- holdDyn (config ^. initialValue) (config ^. setValue)

  (changes, events) <- build $ do
    e <- create
    updateDyn v setter e
    updateFocus e f
    (,) <$> wrapDomEventMaybe e Dom.input (liftIO (getter e))
        <*> bindEvents e

  value <- holdDyn (config ^. initialValue) $
    leftmost [changes, config ^. setValue]

  return $ InputElement value changes events

stringAttr :: String -> Attr String
stringAttr = Attr Just

boolAttr :: String -> Attr Bool
boolAttr = Attr $ \case
  True  -> Just "true"
  False -> Nothing

type_ :: Attr String
type_ = stringAttr "type"

hidden_ :: Attr Bool
hidden_ = boolAttr "hidden"

textInput :: Renderer t => [Attribute t] -> InputConfig t String -> Html t (InputElement t String)
textInput attrs config = do
  dynAttrs <- holdAttributes (attrs ++ [type_ := "text"])
  makeInput Input.setValue Input.getValue config (create dynAttrs)
    where
      create attrs' = Dom.castToHTMLInputElement <$>
        buildElement_ htmlNs "input" attrs'



