{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, NoMonomorphismRestriction, TemplateHaskell #-}

module Reflex.Html.Internal.Attributes where

import Reflex

import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Document as Dom
import qualified GHCJS.DOM.Element  as Dom
import qualified GHCJS.DOM.Node  as Dom

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Data.Bifunctor (second)

import GHC.Exts

import Data.Maybe 
import Data.List

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Functor.Contravariant
import Data.Monoid


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
  


attr :: (Reflex t, MonadHold t m) => (Key, ValueA t m) -> Attributes t m
attr (k, v) = Attributes $ Map.singleton k [v]

instance Monoid (Attributes t m) where
  mempty = Attributes Map.empty
  mappend (Attributes a) (Attributes b) = Attributes $ Map.unionWith (++) a b

overrideA :: (Reflex t, MonadHold t m) => Attributes t m -> Attributes t m -> Attributes t m
overrideA (Attributes a) (Attributes b) = Attributes $ Map.union a b


flattenA :: (Reflex t, MonadHold t m) => Attributes t m -> [(Key, ValueA t m)]
flattenA  = map (second concatValues) . Map.toList . unAttr


   
mergeListDyn :: (Reflex t, MonadHold t m) => [Dynamic t a] -> m (Dynamic t [a])   
mergeListDyn dyns = do 
  lists <- mapM (mapDyn (:[])) dyns 
  mconcatDyn lists
  
  
concatValues :: (Reflex t, MonadHold t m) => [ValueA t m] -> ValueA t m
concatValues values = case dynamic of
  [] -> StaticA $ joinStrs static
  d  -> DynamicA $ do
    values <- sequence d >>= mergeListDyn
    mapDyn (joinStrs . (++ static)) values 
    
  where
    static  = catMaybes $ map (^? staticValue) values
    dynamic = catMaybes $ map (^? dynValue) values
  
    joinStrs = Just . intercalate " " . catMaybes

  
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


  

  
    
  

  
  