module Math.KnotTh.Tangles.Util.Writhe
	(
	  crossingWrithe
	, threadWrithe
	, selfWrithe
	) where

import qualified Data.List as List
import qualified Data.Map as Map

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles.Util.Paths


crossingWrithe :: (Tangle t c d ArbitraryCrossing) => c -> d -> d -> Int
crossingWrithe cr a b
	| cr /= incidentCrossing a || cr /= incidentCrossing b  = error "crossingWrithe: bad incidence"
	| a == nextCCW b  = d
	| a == nextCW b   = -d
	| otherwise       = error "crossingWrithe: something strange"

	where
		d = if passOver b
			then 1
			else -1


threadWrithe :: (Tangle t c d ArbitraryCrossing) => [(d, d)] -> Int
threadWrithe = fst . List.foldl' edgeWrithe (0, Map.empty)
	where
		edgeWrithe (writhe, m) (d, _)
			| isLeg d          = (writhe, m)
			| Map.member cr m  = (writhe + crossingWrithe cr ((Map.!) m cr) d, m)
			| otherwise        = (writhe, Map.insert cr d m)

			where
				cr = incidentCrossing d


selfWrithe :: (Tangle t c d ArbitraryCrossing) => t -> Int
selfWrithe = List.sum . map threadWrithe . allThreads
