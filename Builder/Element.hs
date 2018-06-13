module Builder.Element where


import Prelude
import Reflex.Dom hiding (makeElement, PropertyMap)

import qualified Data.Dependent.Map as DM
import Data.Dependent.Map (DMap, DSum(..))

import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)

import Data.Functor.Identity
import Data.Maybe (catMaybes, fromMaybe)
import Control.Lens ((.~), (&), set)
import Control.Monad (unless)

import Builder.Attribute


type ElemType t m = Element EventResult (DomBuilderSpace m) t

type Elem attr   = forall t m a. (DomBuilder t m, PostBuild t m, AttributeSet attr)  => PropertyMap attr t -> m a  -> m a
type Elem_ attr  = forall t m a. (DomBuilder t m, PostBuild t m, AttributeSet attr)  => PropertyMap attr t -> m () -> m (ElemType t m)
type Elem' attr = forall  t m a. (DomBuilder t m, PostBuild t m, AttributeSet attr)  => PropertyMap attr t -> m a  -> m (ElemType t m, a)
type Child_ attr  = forall t m a. (DomBuilder t m, PostBuild t m, AttributeSet attr) => PropertyMap attr t -> m (ElemType t m)


makeElem ::  Maybe Namespace -> Text -> Elem attr
makeElem ns elemName props child = snd <$> makeElem' ns elemName props child
{-# INLINE makeElem #-}

makeElem_ ::  Maybe Namespace -> Text -> Elem_ attr
makeElem_ ns elemName props child = fst <$> makeElem' ns elemName props child
{-# INLINE makeElem_ #-}

makeChild_ ::  Maybe Namespace -> Text -> Child_ attr
makeChild_ ns elemName props = fst <$> makeElem' ns elemName props (return ())
{-# INLINE makeChild_ #-}

makeElem' :: Maybe Namespace -> Text -> Elem' attr
makeElem' namespace elemName properties child = do

  postBuild <- getPostBuild
  let updates = attributeUpdates postBuild
      config = def
        & elementConfig_namespace         .~ namespace
        & elementConfig_initialAttributes .~ initial
        & if DM.null dynamics then id else elementConfig_modifyAttributes .~ updates

  result <- element elemName config child
  unless (DM.null dynamics) $ notReadyUntil postBuild
  return result

    where
      (statics, dynamics) = splitAttributes (attributeProps properties)
      initial = M.fromList (catMaybes (maybeValue <$> DM.toList statics))

      attributeUpdates postBuild = leftmost [updatedAttrs dynamics, pushAlways (const (sampleAttrs dynamics)) postBuild]


updatedAttrs :: forall t attr. (Reflex t, AttributeSet attr) => DMap attr (Dynamic t) -> Event t (Map AttributeName (Maybe Text))
updatedAttrs d = M.fromList . fmap toValue . DM.toList <$> merge (DM.map updated d) where
  toValue (k :=> Identity v) = valueA k v

sampleAttrs :: forall t attr m. (Reflex t, MonadSample t m, AttributeSet attr) => DMap attr (Dynamic t) -> m (Map AttributeName (Maybe Text))
sampleAttrs d = M.fromList <$> traverse sampleAttr (DM.toList d) where
  sampleAttr (k :=> v) = valueA k <$> sample (current v)


maybeValue :: AttributeSet attr => DSum attr Identity -> Maybe (AttributeName, Text)
maybeValue (k :=> Identity v) = (name,) <$> value where
  (name, value) = valueA k v
