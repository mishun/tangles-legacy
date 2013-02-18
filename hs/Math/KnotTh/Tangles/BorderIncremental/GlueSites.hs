module Math.KnotTh.Tangles.BorderIncremental.GlueSites
	(
	  glueSites
	, positionsToGlue
	) where

import qualified Data.List as List

import qualified Math.Algebra.Group as Group
import qualified Math.Algebra.Group.D4 as D4
import qualified Math.Algebra.Group.Dn as Dn

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.BorderIncremental.BITangle


mirrorRepresentatives :: D4.SubGroup -> D4.Element -> [D4.Element]
mirrorRepresentatives symmetry mirror
	| symmetry == D4.SubD4  = [D4.I]
	| mirror == D4.I        = representatives
	| otherwise             = filter mirrorOk representatives

	where	
		representatives = Group.factorSetRepresentatives symmetry

		mirrorOk g = id1 <= id2
			where
				id1 = Group.factorSetId symmetry g
				id2 = Group.factorSetId symmetry $ mirror Group.<*> g


glueSites :: (CrossingType ct) => (BITangle ct -> (Int, BIDart ct) -> [ct]) -> BITangle ct -> [((ct, D4.Element), (Int, BIDart ct))]
glueSites crossingsLister tangle = List.concatMap positionSites positions
	where
		positions = [ (is, (gl, base)) | gl <- [1 .. 3], (base, is) <- positionsToGlue tangle gl ]

		positionSites (is, pos) = zip (List.concatMap rotations crs) (List.repeat pos)
			where
				crs = crossingsLister tangle pos

				rotations cr = zip (List.repeat cr) (mirrorRepresentatives local is)
					where
						local = localSymmetry cr


positionsToGlue :: (CrossingType ct) => BITangle ct -> Int -> [(BIDart ct, D4.Element)]
positionsToGlue tangle gl =
	if Dn.hasReflectionElement symmetry
		then fill (fst left) (left, right)
		else zip (take period $ List.iterate nextCCW $ firstLeg tangle) (List.repeat D4.I)

	where
		symmetry = tangleSymmetry tangle
		period = Dn.rotationPeriod symmetry
		mirroredZero = mod ((Dn.mirroredZero symmetry) + gl - 1) period

		crossingMirror =
			case gl of
				3 -> D4.EC2
				2 -> D4.EC3
				_ -> D4.E

		getEndpoint doubleIndex = (next legIndex $ firstLeg tangle, induced)
			where
				induced =
					if mod doubleIndex 2 == 0
						then fixup Group.<*> crossingMirror
						else D4.I

				legIndex = quot doubleIndex 2

				gr = Dn.D $ numberOfLegs tangle

				fixup = Dn.adjointDifferenceForBasis (globalTransformBasis tangle) symmetry
						(Dn.fromRotationReflection gr (legIndex - gl + 1, True))
						(Dn.fromRotation gr legIndex)


		left = getEndpoint (mirroredZero - period)
		right = getEndpoint mirroredZero

		fill c (l, r)
			| c == fst r  = [r] -- sic!
			| c == fst l  = l : fill (nextCCW c) (l, r)
			| otherwise   = (c, D4.I) : fill (nextCCW c) (l, r)
