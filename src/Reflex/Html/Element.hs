{-# LANGUAGE TemplateHaskell #-}
module Reflex.Html.Element where

import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.Document as Doc
import qualified GHCJS.DOM.Node as Dom

import qualified GHCJS.DOM.HTMLInputElement as Input

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

inputText :: Reflex t => Element t -> Event t DomString
inputText = domEvent TextInput

buildElement_ :: Renderer t => DomString -> DomString -> Builder t Dom.Element
buildElement_ ns tag = withParent $ \parent doc -> do
  Just e <- liftIO $ Doc.createElementNS doc (Just ns) (Just tag)
  liftIO $ Dom.appendChild parent $ Just e
  return e


buildElement :: Renderer t => DomString -> DomString -> Builder t () -> Builder t Dom.Element
buildElement ns tag (Build child) = do
  dom <- buildElement_ ns tag
  Build $ local (reParent (Dom.toNode dom)) child
  return dom

  where
    reParent dom e = e { envParent = dom }





data Value t a = StaticV a | EventV (Event t a) | DynV   (Dynamic t a)

mapValue :: MonadReflex t m => (a -> b) -> Value t a -> m (Value t b)
mapValue f (StaticV a)  = return $ StaticV (f a)
mapValue f (EventV e)   = return $ EventV (f <$> e)
mapValue f (DynV d)     = DynV <$> mapDyn f d


data Property t where
  Property :: Target a -> Value t a -> Property t

data Target a where
  Attribute     :: Attribute a -> Target a
  SetFocus      :: Target Bool
  SetTextInput  :: Target DomString

class IsTarget target a | target -> a where
  toTarget :: target -> Target a

instance IsTarget (Attribute a) a where
  toTarget = Attribute

instance IsTarget (Target a) a where
  toTarget = id

infixr 0 =:, -:, ~:

(~:) :: IsTarget target a => target -> Dynamic t a -> Property t
(~:) t = Property (toTarget t) . DynV

(=:) :: IsTarget target a => target -> a -> Property t
(=:) t = Property (toTarget t) . StaticV

(-:) :: IsTarget target a => target -> Event t a -> Property t
(-:) t = Property (toTarget t) . EventV

buildProperty :: Renderer t =>  Property t -> Builder t Dom.Element -> Html t (Builder t Dom.Element)
buildProperty (Property target value) = buildProperty_ target value

setAttribute :: DomString -> Dom.Element -> Maybe DomString -> IO ()
setAttribute name elem (Just v) = Dom.setAttribute elem name v
setAttribute name elem Nothing  = Dom.removeAttribute elem name

buildProperty_ :: Renderer t =>  Target a -> Value t a -> Builder t Dom.Element -> Html t (Builder t Dom.Element)
buildProperty_ (Attribute attr) = \value build -> do
  str <- mapValue (attrString attr) value
  setter (setAttribute (attrName attr)) str build

buildProperty_ SetFocus = setter $ \elem b -> case b of
  True  -> Dom.focus elem
  False -> Dom.blur elem

buildProperty_ SetTextInput = setter $ \elem str ->
  Input.setValue (Dom.castToHTMLInputElement elem) (Just str)

setter :: Renderer t => (Dom.Element -> a -> IO ()) -> Value t a -> Builder t Dom.Element -> Html t (Builder t Dom.Element)
setter f (StaticV a)  = setStatic f a
setter f (EventV  e)  = setEvent f e
setter f (DynV d)     = setDyn f d

setEvent :: Renderer t => (Dom.Element -> a -> IO ()) -> Event t a -> Builder t Dom.Element -> Html t (Builder t Dom.Element)
setEvent f e build = do
  initial <- hold Nothing (Just <$> e)
  return $ build >>= \elem -> do
    sample initial >>= traverse (liftIO . f elem)
    elem <$ render e (liftIO . f elem)

setStatic :: Renderer t => (Dom.Element -> a -> IO ()) -> a -> Builder t Dom.Element -> Html t (Builder t Dom.Element)
setStatic f a build = pure $ build >>= \elem -> elem <$ liftIO (f elem a)

setDyn :: Renderer t => (Dom.Element -> a -> IO ()) -> Dynamic t a -> Builder t Dom.Element -> Html t (Builder t Dom.Element)
setDyn  f d build = do
  return $ build >>= \elem -> do
    sample (current d) >>= liftIO . f elem
    elem <$ render (updated d) (liftIO . f elem)


data Attribute a = MkAttr
  { attrString :: (a -> Maybe DomString)
  , attrName   :: DomString
  }

instance Contravariant Attribute where
  contramap f (MkAttr toStr name) = MkAttr (toStr . f) name

option :: Attribute a -> Attribute (Maybe a)
option (MkAttr toStr name) = MkAttr (>>= toStr) name where

manySep :: DomString -> Attribute a -> Attribute [a]
manySep sep (MkAttr toStr name) = MkAttr  toStr' name where
  toStr' xs = case (catMaybes (toStr <$> xs)) of
      []  -> Nothing
      strs -> Just $ S.intercalate sep strs

commaSep :: Attribute a -> Attribute [a]
commaSep = manySep ","

spaceSep :: Attribute a -> Attribute [a]
spaceSep = manySep " "

showA :: Show a => DomString -> Attribute a
showA = contramap domShow . strA

strA :: DomString -> Attribute DomString
strA name = MkAttr Just name

commaListA :: DomString -> Attribute [DomString]
commaListA = commaSep . strA

spaceListA :: DomString -> Attribute [DomString]
spaceListA = spaceSep . strA

boolA :: DomString -> Attribute Bool
boolA = MkAttr (\b -> if b then Just "" else Nothing)

intA :: DomString -> Attribute Int
intA = showA

floatA :: DomString -> Attribute Float
floatA = showA

boolA' :: DomString -> DomString -> DomString -> Attribute Bool
boolA' t f = contramap fromBool . strA
  where fromBool b = if b then t else f


data ElementType = ElementType
  { elemNs    :: DomString
  , elemTag   :: DomString
  }


text :: Renderer t => DomString -> Html t ()
text str = build_ $ void $ buildText str

dynText :: Renderer t => Dynamic t DomString -> Html t ()
dynText d = build_ $ do
  text <- sample (current d) >>= buildText
  void $ render (updated d) $ updateText text

el_ :: Renderer t => ElementType -> [Property t] -> Html t () -> Html t (Element t)
el_ tag attrs = fmap fst . el' tag attrs

elemBuilder :: Renderer t => ElementType -> [Property t] -> Html t a -> Html t (a, Builder t Dom.Element)
elemBuilder tag properties child = do
  (a, r) <- runChild child
  buildElem <- foldrM ($) (buildElement (elemNs tag) (elemTag tag) r) (buildProperty <$> properties)
  return (a, buildElem)

el :: Renderer t => ElementType -> [Property t] -> Html t a -> Html t a
el tag properties child = do
  (a, buildElem) <- elemBuilder tag properties child
  build_ $ void $ buildElem
  return a


el' :: Renderer t => ElementType -> [Property t] -> Html t a -> Html t (Element t, a)
el' tag properties child = do
  (a, buildElem) <- elemBuilder tag properties child
  events <- switchBuild $ buildElem >>= bindEvents
  return (Element events, a)

