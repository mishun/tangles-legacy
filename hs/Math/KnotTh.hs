module Math.KnotTh
	(
	  CrossingType(..)
	, Knotted(..)

	, stateRelativeToDart
	, globalCrossingsTransforms
	, next
	, incidentDarts
	, adjacentDarts
	, incidentCrossing
	, adjacentCrossing
	, allCrossings
	, allDarts
	) where

import qualified Data.Ix as Ix
import qualified Data.List as List

import qualified Math.Algebra.RotationDirection as RotationDirection
import qualified Math.Algebra.Group as Group
import qualified Math.Algebra.Group.D4 as D4


class CrossingType crossingType where
	identifier       :: crossingType -> Int
	localSymmetry    :: crossingType -> D4.SubGroup
	globalTransforms :: crossingType -> D4.SubGroup


class (CrossingType ct, Ix.Ix crossing, Ix.Ix dart) => Knotted knot crossing dart ct
		| knot -> crossing
		, crossing -> dart
		, dart -> knot
		, knot -> ct

	where
		nextCW   :: dart -> dart
		nextCCW  :: dart -> dart
		opposite :: dart -> dart
		begin    :: dart -> (crossing, Int)

		index :: crossing -> Int
		state :: crossing -> (ct, D4.Element)

		numberOfCrossings :: knot -> Int

		crossingsRange :: knot -> (crossing, crossing)
		dartsRange     :: knot -> (dart, dart)

		crossing :: Int -> knot     -> crossing
		dart     :: Int -> crossing -> dart


{-# INLINE stateRelativeToDart #-}
stateRelativeToDart :: (Knotted k c d ct) => RotationDirection.RotationDirection -> d -> (ct, D4.Element)
stateRelativeToDart dir d = crs `seq` h `seq` (crs, h)
	where
		(cr, place) = begin d

		(crs, g) = state cr

		p = D4.fromReflectionRotation (RotationDirection.isClockwise dir, -place)

		h = p Group.<*> g


globalCrossingsTransforms :: (Knotted k c d ct) => k -> D4.SubGroup
globalCrossingsTransforms knot =
	if n == 0
		then D4.SubID
		else globalTransforms st

	where
		n = numberOfCrossings knot

		cr = crossing 0 knot

		(st, _) = state cr


next :: (Integral int, Knotted k c d ct) => int -> d -> d
next steps d =
	if steps >= 0
		then List.genericIndex (List.iterate nextCCW d) steps
		else List.genericIndex (List.iterate nextCW d) (-steps)


{-# INLINE incidentDarts #-}
incidentDarts :: (Knotted k c d ct) => c -> [d]
incidentDarts c = [dart i c | i <- [0 .. 3]]


{-# INLINE adjacentDarts #-}
adjacentDarts :: (Knotted k c d ct) => c -> [d]
adjacentDarts c = map (\ i -> opposite $ dart i c) [0 .. 3]


{-# INLINE incidentCrossing #-}
incidentCrossing :: (Knotted k c d ct) => d -> c
incidentCrossing = fst . begin


{-# INLINE adjacentCrossing #-}
adjacentCrossing :: (Knotted k c d ct) => d -> c
adjacentCrossing = fst . begin . opposite


{-# INLINE allCrossings #-}
allCrossings :: (Knotted k c d ct) => k -> [c]
allCrossings knot = [crossing i knot | i <- [1 .. numberOfCrossings knot]]


{-# INLINE allDarts #-}
allDarts :: (Knotted k c d ct) => k -> [d]
allDarts knot = [dart j (crossing i knot) | i <- [1 .. numberOfCrossings knot], j <- [0 .. 3]]
