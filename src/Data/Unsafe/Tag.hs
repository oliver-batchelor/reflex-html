module Data.Unsafe.Tag (Tag (..)) where


import Data.GADT.Compare
import Data.GADT.Show
import Unsafe.Coerce

newtype Tag a = Tag Int deriving (Eq, Ord)

instance Show (Tag a) where showsPrec p (Tag u) = showsPrec p u
instance GShow (Tag)  where gshowsPrec = showsPrec
instance GEq Tag where
    geq (Tag a) (Tag b)
        | a == b    = Just (unsafeCoerce Refl)
        | otherwise = Nothing

instance GCompare Tag where
    gcompare (Tag a) (Tag b) = case compare a b of
        LT -> GLT
        EQ -> unsafeCoerce (GEQ :: GOrdering () ())
        GT -> GGT
