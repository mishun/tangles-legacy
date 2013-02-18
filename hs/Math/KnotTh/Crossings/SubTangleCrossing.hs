module Math.KnotTh.Crossings.SubTangleCrossing
	(
	  DirectSumDecompositionType(..)
	, transformDirectSumDecomposition

	, SubTangleCrossing
	, extractTangle
	, containingCrossings
	, numberOfSubstitutedCrossings
	, fromTangle
	, directSumDecompositionState
	, substituteSubTangles
	) where

import qualified Data.List as List
import qualified Data.Array as Array

import qualified Math.Algebra.Group as Group
import qualified Math.Algebra.Group.D4 as D4
import qualified Math.KnotTh.Tangles.TangleSt as TangleSt

import Math.KnotTh
import Math.KnotTh.Tangles


data DirectSumDecompositionType = Loner | DirectSum01_23 | DirectSum12_30 | NoDirectSum deriving (Eq)


transformDirectSumDecomposition :: D4.Element -> DirectSumDecompositionType -> DirectSumDecompositionType
transformDirectSumDecomposition g ds =
	if isChange
		then change ds
		else ds

	where
		image01 = (Group.permute g 0, Group.permute g 1)

		isChange = (image01 == (1, 2)) || (image01 == (2, 1)) || (image01 == (0, 3)) || (image01 == (3, 0))

		change Loner = Loner
		change DirectSum01_23 = DirectSum12_30
		change DirectSum12_30 = DirectSum01_23
		change NoDirectSum = NoDirectSum


data SubTangleCrossing t = SubT Int D4.SubGroup DirectSumDecompositionType t


instance CrossingType (SubTangleCrossing t) where
	identifier (SubT ids _ _ _) = ids

	localSymmetry (SubT _ sym _ _) = sym

	globalTransforms _ = D4.SubID


instance Show (SubTangleCrossing t) where
	show st = List.concat ["sub(", show $ identifier st, ")"]


extractTangle :: (Tangle t c d ct) => SubTangleCrossing t -> t
extractTangle (SubT _ _ _ tangle) = tangle


containingCrossings :: (Tangle t c d ct) => SubTangleCrossing t -> Int
containingCrossings = numberOfCrossings . extractTangle


numberOfSubstitutedCrossings :: (Tangle st sc sd ct, Knotted t c d (SubTangleCrossing st)) => t -> Int
numberOfSubstitutedCrossings tangle = List.sum $ map (containingCrossings . fst . state) $ allCrossings tangle


fromTangle :: (Tangle t c d ct) => Int -> D4.SubGroup -> DirectSumDecompositionType -> t -> SubTangleCrossing t
fromTangle idx symmetry directSumType tangle
	| l /= 4     = error "fromTangle: tangle must have 4 legs"
	| otherwise  = SubT idx symmetry directSumType tangle

	where
		l = numberOfLegs tangle


directSumDecompositionState :: (Tangle t c d ct) => SubTangleCrossing t -> D4.Element -> DirectSumDecompositionType
directSumDecompositionState (SubT _ _ ds _) g = transformDirectSumDecomposition g ds


substituteSubTangles :: (Tangle intT intC intD ct, Tangle extT extC extD (SubTangleCrossing intT)) => extT -> TangleSt.TangleSt ct
substituteSubTangles tangle = result
	where
		n = numberOfCrossings tangle

		internals = Array.listArray (1, n) $
			let transformSubtangle (cr, g) = TangleSt.cloneTransform (D4.toDnElement g) (extractTangle cr)
			in map (transformSubtangle . state) $ allCrossings tangle

		offset = Array.listArray (1, n) $
			let addSize i (st, _) = i + containingCrossings st
			in scanl addSize 0 $ map state $ allCrossings tangle

		intOpposite t u =
			if isLeg v
				then extOpposite $ dart (legPosition v) t
				else ((offset Array.! (index t)) + index c, place)

			where
				v = opposite u

				(c, place) = begin v

		extOpposite u =
			if isLeg v
				then (0, legPosition v)
				else intOpposite c $ (allLegs $ internals Array.! (index c)) !! place

			where
				v = opposite u

				(c, place) = begin v

		result = TangleSt.constructFromList (border : connections, states)
			where
				border = map extOpposite $ allLegs tangle

				connections = List.concatMap fromCrossing $ allCrossings tangle
					where
						fromCrossing c = map (map (intOpposite c) . incidentDarts) $ allCrossings t
							where
								t = internals Array.! (index c)

				states = List.concatMap (map state . allCrossings) $ Array.elems internals
