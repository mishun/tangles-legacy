module Math.Surf.Graph.Dart
	(
	  Dart(..)
	) where

import qualified Data.Ord as Ord
import qualified Data.Ix as Ix

import qualified Data.IxWith as IxWith


data Dart owner = Dart { _dartOwner :: owner, _dartIndex :: Int }


instance Eq (Dart g) where
	(==) a b = (_dartIndex a == _dartIndex b)


instance Ord (Dart g) where
	compare = Ord.comparing _dartIndex


instance Ix.Ix (Dart g) where
	range (a, b) = IxWith.rangeWith (_dartIndex, Dart (_dartOwner a)) (a, b)

	index = IxWith.indexWith _dartIndex

	inRange = IxWith.inRangeWith _dartIndex

	rangeSize = IxWith.rangeSizeWith _dartIndex
