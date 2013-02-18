module Math.KnotTh.Crossings.ArbitraryCrossing
	(
	  ArbitraryCrossing(..)

	, passOver
	, passUnder
	, passOverIndex
	, passUnderIndex
	) where

import qualified Math.Algebra.RotationDirection as RotDir
import qualified Math.Algebra.Group as Group
import qualified Math.Algebra.Group.D4 as D4

import Math.KnotTh


data ArbitraryCrossing = ArbitraryCrossing


instance CrossingType ArbitraryCrossing where
	identifier _ = 1

	localSymmetry _ = D4.SubDS

	globalTransforms _ = D4.SubECS


instance Show ArbitraryCrossing where
	show _ = "-|-"


passOver :: (Knotted k c d ArbitraryCrossing) => d -> Bool
passOver d =
	let (_, g) = stateRelativeToDart RotDir.CounterClockwise d
	in (mod (Group.permute g 0) 2) == 0


passUnder :: (Knotted k c d ArbitraryCrossing) => d -> Bool
passUnder = not . passOver


passOverIndex :: (ArbitraryCrossing, D4.Element) -> Int -> Bool
passOverIndex (_, g) i = mod (Group.permute g i) 2 == 0


passUnderIndex :: (ArbitraryCrossing, D4.Element) -> Int -> Bool
passUnderIndex st i = not $ passOverIndex st i
