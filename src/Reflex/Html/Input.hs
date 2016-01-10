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
import Reflex.Html.Element
import Reflex.Html.Prelude

import qualified Data.Map as Map
import Control.Lens

-- Value of an element
class Reflex t => Value t f | f -> t where
  value   :: f a -> Dynamic t a
  changed :: f a -> Event t a   -- Change events from user input (not programatically changed)

class Reflex t => SetValue t f | f -> t where
  setValue      :: Lens' (f a) (Event t a)
  initialValue  :: Lens' (f a) a

instance Default DomString where
  def = ""

class Reflex t => SetFocus t f | f -> t where
  setFocus :: Lens' (f a) (Event t Bool)


data InputElement t a = InputElement
    { inputValue :: Dynamic t a
    , inputChanged :: Event t a
    , inputEvents :: Events t
    }

instance Reflex t => DomEvents t (InputElement t a) where
  domEvent en e = domEvent_ en (inputEvents e)

data InputConfig t a = InputConfig
    { _confInitialValue :: a
    , _confSetValue :: Event t a
    , _confSetFocus :: Event t Bool
    }

liftM concat $ mapM makeLenses
  [ ''InputConfig ]

instance (Default a, Reflex t) => Default (InputConfig t a) where
  def = InputConfig def def def

instance Reflex t => SetValue t (InputConfig t)  where
  setValue = confSetValue
  initialValue = confInitialValue

instance Reflex t => Value t (InputElement t) where
  value = inputValue
  changed = inputChanged

instance Reflex t => SetFocus t (InputConfig t) where
  setFocus = confSetFocus

updateFocus :: (Renderer t, Dom.IsElement e) => e -> Dynamic t Bool ->  Builder t ()
updateFocus e focus = do
  initial <- sample (current focus)
  void $ render (updated focus) $ \case
      True  -> liftIO $ Dom.focus e
      False -> liftIO $ Dom.blur e

updateDyn :: Renderer t => Dynamic t a -> (e -> Maybe a -> IO ()) -> e -> Builder t ()
updateDyn d setter e = do
  liftIO . setter e . Just =<< sample (current d)
  void $ render (updated d) $ liftIO . setter e . Just


makeInput :: (MonadWidget t m, Dom.IsElement e) => (e -> Maybe a -> IO ()) -> (e -> IO (Maybe a))
              -> InputConfig t a -> Builder t e -> m (InputElement t a)
makeInput setter getter config create = do
  f <- holdDyn False (config ^. setFocus)
  v <- holdDyn (config ^. initialValue) (config ^. setValue)

  (changes, events) <- switchBuild $ do
    e <- create
    updateDyn v setter e
    updateFocus e f
    (,) <$> wrapDomEventMaybe e Dom.input (liftIO (getter e))
        <*> bindEvents e

  value <- holdDyn (config ^. initialValue) $
    leftmost [changes, config ^. setValue]

  return $ InputElement value changes events

stringAttr :: DomString -> Attr DomString
stringAttr = Attr Just

boolAttr :: DomString -> Attr Bool
boolAttr = Attr $ \case
  True  -> Just "true"
  False -> Nothing

type_ :: Attr DomString
type_ = stringAttr "type"

hidden_ :: Attr Bool
hidden_ = boolAttr "hidden"

textInput :: MonadWidget t m => [Attribute t] -> InputConfig t DomString -> m (InputElement t DomString)
textInput attrs config = do
  dynAttrs <- holdAttributes (attrs ++ [type_ := "text"])
  makeInput Input.setValue Input.getValue config (create dynAttrs)
    where
      create attrs' = Dom.castToHTMLInputElement <$>
        buildElement_ htmlNs "input" attrs'



