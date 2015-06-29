{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction, InstanceSigs #-}

module Reflex.Dom.Html.Internal.Attributes where

import Reflex
import Reflex.Dom

import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Element  as Dom
import qualified GHCJS.DOM.Node  as Dom

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Data.Bifunctor (second)


import Data.Maybe 
import Data.List
import qualified Data.Map as Map
import Data.Functor.Contravariant



type Key = String
data ValueA t = StaticA (Maybe String) |  DynamicA (Dynamic t (Maybe String))

-- If Dynamic becomes a Functor we can remove the "m"
type Attribute t m = (Key, m (ValueA t))
data Attr a = Attr { 
  _attr_key :: Key, 
  _attr_map :: a -> Maybe String 
}

instance Contravariant Attr where
  contramap f (Attr k m) = Attr k (m . f)


stringAttr :: Key -> Attr String
stringAttr key = Attr key Just

showAttr :: Show a => Key -> Attr a
showAttr key = Attr key (Just . show)

boolAttr :: Key -> Attr Bool
boolAttr key = Attr key bool where
  bool True  = Just ""
  bool False = Nothing

makeGroups :: Ord k => [(k, v)] -> [(k, [v])]
makeGroups  = Map.toList . Map.fromListWith (++) . map (second pure)

  
concatValues :: (MonadHold t m, Reflex t) =>  [m (ValueA t)] -> m (ValueA t)
concatValues g = do 
  (v:vs) <- sequence g
  foldM catValues v vs 
  
-- Make attributes concatenable, if a key is specified twice then the attribute values are concatenated
catValues :: (MonadHold t m, Reflex t) =>  ValueA t -> ValueA t -> m (ValueA t)
catValues (StaticA v) (StaticA v') = return $ StaticA $ catValues' v v'
catValues (DynamicA d) (StaticA v) = DynamicA <$> mapDyn (catValues' v) d 
catValues (DynamicA d) (DynamicA d') = DynamicA <$> combineDyn catValues' d d' 
catValues v1 v2 = catValues v2 v1
  
catValues' :: Maybe String -> Maybe String -> Maybe String
catValues' v v' =  case catMaybes [v, v'] of
  [] -> Nothing
  l  -> Just $ intercalate " " l
  
    
  

  
  