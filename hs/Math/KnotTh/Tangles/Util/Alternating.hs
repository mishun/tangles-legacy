module Math.KnotTh.Tangles.Util.Alternating
	(
	  isAlternating
	) where

import qualified Data.List as List

import qualified Math.KnotTh.Crossings.ArbitraryCrossing as ArbC

import Math.KnotTh.Tangles


isAlternating :: (Tangle t c d ArbC.ArbitraryCrossing) => t -> Bool
isAlternating = List.all altOrBorderEdge . allEdges
	where
		altOrBorderEdge (a, b) = isLeg a || isLeg b || ArbC.passOver a == ArbC.passUnder b


--projectionToAlternating :: (Tangle t c d ProjC.ProjectionCrossing) -> TangleSt.TangleSt ArbC.ArbitraryCrossing
--projectionToAlternating projection = TangleSt.constructFromList $ toList projection -- TODO: implement
