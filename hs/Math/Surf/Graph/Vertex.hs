module Math.Surf.Graph.Vertex
	(
	  Vertex(..)
	) where

import qualified Data.Ord as Ord
import qualified Data.Ix as Ix

import qualified Data.IxWith as IxWith


data Vertex owner = Vertex { _vertexOwner :: owner, _vertexIndex :: Int }


instance Eq (Vertex g) where
	(==) a b = (_vertexIndex a == _vertexIndex b)


instance Ord (Vertex g) where
	compare = Ord.comparing _vertexIndex


instance Ix.Ix (Vertex g) where
	range (a, b) = IxWith.rangeWith (_vertexIndex, Vertex (_vertexOwner a)) (a, b)

	index = IxWith.indexWith _vertexIndex

	inRange = IxWith.inRangeWith _vertexIndex

	rangeSize = IxWith.rangeSizeWith _vertexIndex
