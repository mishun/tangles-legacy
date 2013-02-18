module Math.KnotTh.Tangles
	(
	  Tangle(..)

	, numberOfEdges
	, continuation
	, adjacentToBorder
	, firstLeg
	, allLegs
	, allLegsAndDarts
	, allEdges
	, maybeIncidentCrossing
	, maybeAdjacentCrossing
	) where

import qualified Data.List as List

import Math.KnotTh


class (Knotted tangle crossing dart ct) => Tangle tangle crossing dart ct
		| tangle -> crossing
		, crossing -> dart
		, dart -> tangle
		, tangle -> ct

	where
		isDart      :: dart -> Bool
		isLeg       :: dart -> Bool
		legPosition :: dart -> Int

		numberOfLegs :: tangle -> Int
		legsRange    :: tangle -> (dart, dart)


numberOfEdges :: (Tangle t c d ct) => t -> Int
numberOfEdges tangle = 2 * (numberOfCrossings tangle) + div (numberOfLegs tangle) 2


continuation :: (Tangle t c d ct) => d -> d
continuation d
	| isDart d   = nextCCW $ nextCCW d
	| otherwise  = error "continuation from leg"


adjacentToBorder :: (Tangle t c d ct) => d -> Bool
adjacentToBorder = isLeg . opposite


firstLeg :: (Tangle t c d ct) => t -> d
firstLeg = fst . legsRange


{-# INLINE allLegs #-}
allLegs :: (Tangle t c d ct) => t -> [d]
allLegs tangle = List.take l $ List.iterate nextCCW first
	where
		l = numberOfLegs tangle
		first = firstLeg tangle


allLegsAndDarts :: (Tangle t c d ct) => t -> [d]
allLegsAndDarts tangle = (allLegs tangle) ++ (allDarts tangle)


allEdges :: (Tangle t c d ct) => t -> [(d, d)]
allEdges tangle = filter (\ (a, b) -> a < b) es
	where
		es = map (\ d -> (d, opposite d)) $ allLegsAndDarts tangle


maybeIncidentCrossing :: (Tangle t c d ct) => d -> Maybe c
maybeIncidentCrossing d =
	if isLeg d
		then Nothing
		else Just $ fst $ begin d


maybeAdjacentCrossing :: (Tangle t c d ct) => d -> Maybe c
maybeAdjacentCrossing d =
	if isLeg o
		then Nothing
		else Just $ fst $ begin o

	where
		o = opposite d
