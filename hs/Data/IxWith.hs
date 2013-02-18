module Data.IxWith
	(
	  equalWith
	, rangeWith
	, indexWith
	, inRangeWith
	, rangeSizeWith
	) where

import qualified Data.Eq as Eq
import qualified Data.Ix as Ix


equalWith :: (Eq.Eq b) => (a -> b) -> a -> a -> Bool
equalWith f a b = (f a) == (f b)


rangeWith :: (Ix.Ix b) => (a -> b, b -> a) -> (a, a) -> [a]
rangeWith (f, g) (a, b) = map g $ Ix.range (f a, f b)


indexWith :: (Ix.Ix b) => (a -> b) -> (a, a) -> a -> Int
indexWith f (a, b) i = Ix.index (f a, f b) $ f i


inRangeWith :: (Ix.Ix b) => (a -> b) -> (a, a) -> a -> Bool
inRangeWith f (a, b) i = Ix.inRange (f a, f b) $ f i


rangeSizeWith :: (Ix.Ix b) => (a -> b) -> (a, a) -> Int
rangeSizeWith f (a, b) = Ix.rangeSize (f a, f b)
