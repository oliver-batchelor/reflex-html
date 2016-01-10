{-# LANGUAGE TemplateHaskell #-}
module Reflex.Html.Element where

import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Element as Dom

import Reflex.Html.Html
import Reflex.Html.Render
import Reflex.Html.Event

import qualified Data.Map as Map

import Control.Lens
import Data.Functor.Misc
import Data.Functor.Contravariant

import Reflex.Html.Prelude
import qualified Reflex.Html.DomString as S

class DomEvents t a where
  domEvent :: EventName en -> a -> Event t (EventResultType en)

newtype Element t = Element { elemEvents :: Events t }

instance Reflex t => DomEvents t (Element t) where
  domEvent en e = domEvent_ en (elemEvents e)

domEvent_ :: Reflex t => EventName en -> Events t -> Event t (EventResultType en)
domEvent_ en events = unEventResult <$> select events (WrapArg en)


clicked :: Reflex t => Element t -> Event t ()
clicked  = domEvent Click


fmapDyn :: Reflex t => (a -> b) -> Dynamic t a -> Dynamic t b
fmapDyn f d = unsafeDynamic (f <$> current d) (f <$> updated d)

data Attribute t = forall a. (:=) (Attr a) a
                 | forall a. (:~) (Attr a) (Dynamic t a)

data Attr a = Attr
  { attrString :: (a -> Maybe DomString)
  , attrName   :: DomString
  }

instance Contravariant Attr where
  contramap f (Attr toStr name) = Attr (toStr . f) name

option :: Attr a -> Attr (Maybe a)
option (Attr toStr name) = Attr (>>= toStr) name where

many :: Attr a -> Attr [a]
many (Attr toStr name) =  Attr toStr' name where
  toStr' xs = case (catMaybes (toStr <$> xs)) of
      []  -> Nothing
      strs -> Just $ S.intercalate " " strs


data ElementType = ElementType
  { elemNs    :: DomString
  , elemTag   :: DomString
  }


sampleAttributes :: MonadSample t m => Map DomString (Either (Maybe DomString) (Dynamic t (Maybe DomString))) -> m (Map DomString DomString)
sampleAttributes attrMap = do
 strs <- forM attrMap $ \case
   Left  str -> pure str
   Right d   -> sample (current d)
 return (Map.mapMaybe id strs)


holdAttributes :: MonadReflex t m => [Attribute t] -> m (DynMap t DomString DomString)
holdAttributes attrs = do
  attrMap <- Map.fromList <$> traverse toString attrs
  return (pull $ sampleAttributes attrMap,
         mergeMap $ Map.mapMaybe (fmap updated . fromRight) attrMap)

  where
    toString (Attr f k := a) = pure (k, Left (f a))
    toString (Attr f k :~ d) = (k,) . Right <$> mapDyn f d

    fromRight (Right a) = Just a
    fromRight _         = Nothing

htmlNs :: DomString
htmlNs = "http://www.w3.org/1999/xhtml"

text :: MonadWidget t m => DomString -> m ()
text str = build $ void $ buildText str

dynText :: MonadWidget t m => Dynamic t DomString -> m ()
dynText d = build $ do
  text <- sample (current d) >>= buildText
  void $ render (updated d) $ updateText text


el_ :: MonadWidget t m => ElementType -> [Attribute t] -> m () -> m (Element t)
el_ e attrs = fmap snd . el' e attrs

el :: MonadWidget t m => ElementType -> [Attribute t] -> m a -> m a
el e attrs child = do
  (a, r) <- collectBuild child
  dynAttrs <- holdAttributes attrs
  build $ void $ buildElement (elemNs e) (elemTag e) dynAttrs r
  return a

el' :: MonadWidget t m => ElementType -> [Attribute t] -> m a -> m (a, Element t)
el' e attrs child = do
  (a, r) <- collectBuild child
  dynAttrs <- holdAttributes attrs
  events <- switchBuild $
      buildElement (elemNs e) (elemTag e) dynAttrs r >>= bindEvents . fst
  return (a, Element events)

