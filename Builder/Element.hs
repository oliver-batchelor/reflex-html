module Builder.Element where


import Prelude
import Reflex.Dom hiding (makeElement)
import Reflex.Active

import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.Text (Text)

import Data.Maybe (catMaybes)
import Control.Lens ((.~), (&))
import Control.Monad (unless)

import Builder.Attribute

type ElemType t m = Element EventResult (DomBuilderSpace m) t
type ElemConfig t m = ElementConfig EventResult t (DomBuilderSpace m)

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

{-# INLINE configure #-}
configure :: forall t m a. (DomBuilder t m, PostBuild t m)  
          => Maybe Namespace -> [Property t] -> m (ElemConfig t m)
configure namespace props = do
  postBuild <- getPostBuild

  let updates = attrUpdates postBuild
      config = def
        & elementConfig_namespace         .~ namespace
        & elementConfig_initialAttributes .~ attrInitial
        & case updates of
              [] -> id
              es -> elementConfig_modifyAttributes  .~ mconcat updates

    
  unless (null updates) $ notReadyUntil postBuild
  return config

    where

      attrInitial = mconcat $ ffor props $ \case
        AttrProp attr (Static a) -> applyAttr' attr a
        _ -> mempty

      attrUpdates :: Event t () -> [Event t (Map AttributeName (Maybe Text))]
      attrUpdates postBuild = catMaybes $ ffor props $ \case
        AttrProp attr (Dyn d) ->
          Just (applyAttr attr <$> leftmost [updated d, tag (current d) postBuild])
        _ -> Nothing


{-# INLINE makeElem' #-}
makeElem' :: forall t m a. (DomBuilder t m, PostBuild t m)
          => Maybe Namespace -> Text -> [Property t] -> m a  -> m (ElemType t m, a)
makeElem' namespace elemName props child = do

  config <- configure namespace props
  result <- element elemName config child

  return result

