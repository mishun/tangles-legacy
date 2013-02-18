module Math.KnotTh.Tangles.Util.Arbitrary
	(
	  invertCrossings
	) where

import qualified Math.Algebra.Group.D4 as D4

import Math.Algebra.Group
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.TangleSt
import Math.KnotTh.Tangles.Util.List


invertCrossings :: (Tangle t c d ArbitraryCrossing) => t -> TangleSt ArbitraryCrossing
invertCrossings tangle = constructFromList (conn, map (\ (c, g) -> (c, D4.C <*> g)) crs)
	where
		(conn, crs) = toList tangle
