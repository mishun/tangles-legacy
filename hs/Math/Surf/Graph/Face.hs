module Math.Surf.Graph.Face
	(
	  Face(..)
	) where

import qualified Data.Ord as Ord
import qualified Data.Ix as Ix

import qualified Data.IxWith as IxWith


data Face owner = Face { _faceOwner :: owner, _faceIndex :: Int }


instance Eq (Face g) where
	(==) a b = (_faceIndex a == _faceIndex b)


instance Ord (Face g) where
	compare = Ord.comparing _faceIndex


instance Ix.Ix (Face g) where
	range (a, b) = IxWith.rangeWith (_faceIndex, Face (_faceOwner a)) (a, b)

	index = IxWith.indexWith _faceIndex

	inRange = IxWith.inRangeWith _faceIndex

	rangeSize = IxWith.rangeSizeWith _faceIndex
