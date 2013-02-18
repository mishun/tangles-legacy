module Math.KnotTh.Tangles.Util.List
	(
	  toPair
	, toListWith
	, toList
	, toGraph
	) where

import qualified Data.List as List

import qualified Math.Algebra.Group.D4 as D4
import qualified Math.Surf.Graph.GraphSt as GraphSt
import qualified Math.Surf.Graph.Construction as GraphConstruction

import Math.KnotTh
import Math.KnotTh.Tangles


toPair :: (Tangle t c d ct) => d -> (Int, Int)
toPair d =
	if isLeg d
		then (0, legPosition d)
		else (index c, place)

	where
		(c, place) = begin d


toListWith :: (Tangle t c d ct) => (d -> d) -> t -> ([[(Int, Int)]], [(ct, D4.Element)])
toListWith f tangle = (border : map (map toElement . incidentDarts) crs, map state crs)
	where
		toElement = toPair . f

		border = map toElement $ allLegs tangle

		crs = allCrossings tangle


toList :: (Tangle t c d ct) => t -> ([[(Int, Int)]], [(ct, D4.Element)])
toList = toListWith opposite


toGraph :: (Tangle t c d ct) => t -> GraphSt.GraphSt
toGraph tangle = GraphConstruction.constructFromList $ map (map fixup) $ (rev border) : rest
	where
		l = numberOfLegs tangle

		(border : rest) = fst $ toList tangle

		rev [] = []
		rev (first : restList) = first : List.reverse restList

		fixup (i, j) =
			if i == 0
				then (0, mod (l - j) l)
				else (i, j)
