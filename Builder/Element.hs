module Builder.Element where


import Prelude
import Reflex.Dom hiding (makeElement)
import Reflex.Active

import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)

import Data.Maybe (catMaybes)
import Control.Lens ((.~), (&))
import Control.Monad (unless)

import Builder.Attribute

type ElemType t m = Element EventResult (DomBuilderSpace m) t

type Elem   = forall t m a. (DomBuilder t m, PostBuild t m) => [Property t] -> m a  -> m a
type Elem_  = forall t m a. (DomBuilder t m, PostBuild t m) => [Property t] -> m () -> m (ElemType t m)
type Elem'  = forall t m a. (DomBuilder t m, PostBuild t m) => [Property t] -> m a  -> m (ElemType t m, a)
type Child_  = forall t m a. (DomBuilder t m, PostBuild t m) => [Property t] -> m (ElemType t m)


makeElem ::  Maybe Namespace -> Text -> Elem
makeElem ns elemName props child = snd <$> makeElem' ns elemName props child
{-# INLINE makeElem #-}

makeElem_ ::  Maybe Namespace -> Text -> Elem_
makeElem_ ns elemName props child = fst <$> makeElem' ns elemName props child
{-# INLINE makeElem_ #-}

makeChild_ ::  Maybe Namespace -> Text -> Child_
makeChild_ ns elemName props = fst <$> makeElem' ns elemName props (return ())
{-# INLINE makeChild_ #-}

makeElem' :: forall t m a. (DomBuilder t m, PostBuild t m)
          => Maybe Namespace -> Text -> [Property t] -> m a  -> m (ElemType t m, a)
makeElem' namespace elemName properties child = do

  postBuild <- getPostBuild

  let updates = attrUpdates postBuild
      config = def
        & elementConfig_namespace         .~ namespace
        & elementConfig_initialAttributes .~ attrInitial
        & case updates of
              [] -> id
              es -> elementConfig_modifyAttributes  .~ mconcat updates

  result <- element elemName config child
  unless (null updates) $ notReadyUntil postBuild
  return result

  where

    attrInitial = mconcat $ ffor properties $ \case
      AttrProp attr (Static a) -> applyAttr' attr a
      _ -> mempty

    attrUpdates :: Event t () -> [Event t (Map AttributeName (Maybe Text))]
    attrUpdates postBuild = catMaybes $ ffor properties $ \case
      AttrProp attr (Dyn d) ->
        Just (applyAttr attr <$> leftmost [updated d, tag (current d) postBuild])
      _ -> Nothing
