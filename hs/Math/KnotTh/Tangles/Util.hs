module Math.KnotTh.Tangles.Util
	(
	  diskHomeomorphismInvariant
	) where

import qualified Data.List as List

import qualified Math.Algebra.RotationDirection as RotationDirection
import qualified Math.Algebra.Group as Group
import qualified Math.KnotTh.Tangles.Invariants.RootCode as RootCode

import Math.KnotTh
import Math.KnotTh.Tangles


diskHomeomorphismInvariant :: (Tangle t c d ct) => t -> [Int]
diskHomeomorphismInvariant tangle = List.minimum $ map legCode $ allLegs tangle
	where
		legCode leg =
			let global = [ (dir, gt) | dir <- RotationDirection.both, gt <- Group.subGroupElements $ globalCrossingsTransforms tangle ]
			in List.minimum $ map (\ (dir, gt) -> RootCode.rootCodeLeg tangle leg dir gt) global
