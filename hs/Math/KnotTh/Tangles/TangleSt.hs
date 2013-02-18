module Math.KnotTh.Tangles.TangleSt
	(
	  TangleSt
	, clone
	, cloneTransform
	, constructFromList
	, singleCrossingTangle
	) where

import qualified Data.List as List
import qualified Data.Bits as Bits
import qualified Data.Ord as Ord
import qualified Data.Ix as Ix
import qualified Data.Array as Array

import qualified Data.IxWith as IxWith
import qualified Math.Algebra.Group as Group
import qualified Math.Algebra.Group.D4 as D4
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.KnotTh.Tangles.Util.Show as TangleShow
import qualified Math.KnotTh.Tangles.Util.List as TangleList

import Math.KnotTh.Tangles
import Math.KnotTh


data DartSt ct = DartSt
	{
		_dartOwner :: TangleSt ct,
		_dartIndex :: !Int
	}
	| LegSt
	{
		_dartOwner :: TangleSt ct,
		_legIndex :: !Int
	}


data CrossingSt ct = CrossingSt
	{
		_crossingOwner :: TangleSt ct,
		_crossingIndex :: !Int
	}

data TangleSt ct = TangleSt
	{
		_crossings :: !(Array.Array Int (ct, D4.Element)),
		_darts, _legs :: !(Array.Array Int (DartSt ct))
	}


instance (CrossingType ct) => Eq (DartSt ct) where
	(==) (DartSt _ id1) (DartSt _ id2) = (id1 == id2)
	(==) (LegSt _ id1) (LegSt _ id2) = (id1 == id2)
	(==) _ _ = False

instance (CrossingType ct) => Eq (CrossingSt ct) where
	(==) = IxWith.equalWith _crossingIndex


instance (CrossingType ct) => Ord (DartSt ct) where
	compare (DartSt _ id1) (DartSt _ id2) = compare id1 id2
	compare (LegSt _ id1) (LegSt _ id2) = compare id1 id2
	compare (LegSt _ _) (DartSt _ _) = LT
	compare (DartSt _ _) (LegSt _ _) = GT

instance (CrossingType ct) => Ord (CrossingSt ct) where
	compare = Ord.comparing _crossingIndex


instance (CrossingType ct) => Ix.Ix (DartSt ct) where
	range (DartSt o id1, DartSt _ id2) = map (\ i -> DartSt o i) [id1 .. id2]
	range (LegSt o id1, LegSt _ id2)   = map (\ i -> LegSt o i) [id1 .. id2]
	range _ = error "range from leg and dart"

	index (DartSt _ id1, DartSt _ _) (DartSt _ id0) = id0 - id1
	index (LegSt _ id1, LegSt _ _) (LegSt _ id0) = id0 - id1
	index _ _ = error "index from leg and dart"

	inRange (DartSt _ id1, DartSt _ id2) (DartSt _ id0) = (id0 >= id1) && (id0 <= id2)
	inRange (LegSt _ id1, LegSt _ id2) (LegSt _ id0) = (id0 >= id1) && (id0 <= id2)
	inRange _ _ = error "inRange from leg and dart"

	rangeSize (DartSt _ id1, DartSt _ id2) = max 0 (1 + id2 - id1)
	rangeSize (LegSt _ id1, LegSt _ id2) = max 0 (1 + id2 - id1)
	rangeSize _ = error "rangeSize from leg and dart"

instance (CrossingType ct) => Ix.Ix (CrossingSt ct) where
	range (a, b) = IxWith.rangeWith (_crossingIndex, CrossingSt (_crossingOwner a)) (a, b)

	index = IxWith.indexWith _crossingIndex

	inRange = IxWith.inRangeWith _crossingIndex

	rangeSize = IxWith.rangeSizeWith _crossingIndex


instance (CrossingType ct) => Knotted (TangleSt ct) (CrossingSt ct) (DartSt ct) ct where

	nextCCW (DartSt t i) = DartSt t $ (i Bits..&. (Bits.complement 3)) Bits..|. ((i + 1) Bits..&. 3)
	nextCCW (LegSt t i) =
		let l = numberOfLegs t
		in LegSt t $ mod (i + 1) l

	nextCW (DartSt t i) = DartSt t $ (i Bits..&. (Bits.complement 3)) Bits..|. ((i - 1) Bits..&. 3)
	nextCW (LegSt t i) =
		let l = numberOfLegs t
		in LegSt t $ mod (i + l - 1) l

	opposite (DartSt t i) = (_darts t) Array.! i
	opposite (LegSt t i) = (_legs t) Array.! i 

	begin (DartSt t i) = (CrossingSt t $ Bits.shiftR i 2, i Bits..&. 3)
	begin _ = error "begin from leg"

	index (CrossingSt _ i) = i + 1

	state (CrossingSt t i) = (_crossings t) Array.! i

	numberOfCrossings t = 1 + (snd $ Array.bounds $ _crossings t)

	crossingsRange t =
		let n = numberOfCrossings t
		in (CrossingSt t 0, CrossingSt t $ n - 1)

	dartsRange t =
		let n = numberOfCrossings t
		in (DartSt t 0, DartSt t $ 4 * n - 1)

	crossing i t =
		if i < 1 || i > (numberOfCrossings t)
			then error "crossing: index is out of bound"
			else CrossingSt t (i - 1)

	dart d (CrossingSt t i) = DartSt t (4 * i + (d Bits..&. 3))


instance (CrossingType ct) => Tangle (TangleSt ct) (CrossingSt ct) (DartSt ct) ct where

	isDart (DartSt _ _) = True
	isDart (LegSt _ _) = False

	isLeg = not . isDart

	legPosition (LegSt _ i) = i
	legPosition _ = error "legPosition from dart"

	numberOfLegs t = 1 + (snd $ Array.bounds $ _legs t)

	legsRange t =
		let l = numberOfLegs t
		in (LegSt t 0, LegSt t (l - 1))


instance (CrossingType ct, Show ct) => Show (TangleSt ct) where
	show = TangleShow.showTangleStructure


clone :: (Tangle t c d ct) => t -> TangleSt ct
clone tangle = result
	where
		n = numberOfCrossings tangle
		l = numberOfLegs tangle

		copyDart d =
			if isLeg d
				then LegSt result $ legPosition d
				else DartSt result ((index cr - 1) * 4 + pl)

			where
				(cr, pl) = begin d

		result = TangleSt cs ds ls
			where
				cs = Array.listArray (0, n - 1) $ map state $ allCrossings tangle
				ds = Array.listArray (0, 4 * n - 1) $ map (copyDart . opposite) $ allDarts tangle
				ls = Array.listArray (0, l - 1) $ map (copyDart . opposite) $ allLegs tangle


cloneTransform :: (Tangle t c d ct) => Dn.Element -> t -> TangleSt ct
cloneTransform transform tangle
	| not $ orderOk  = error "cloneTransform: order conflict"
	| otherwise      = constructFromList (border' : connections', crs')

	where
		l = numberOfLegs tangle

		orderOk = l == (Group.permutationOrder transform)

		mirror = Dn.hasReflection transform

		ct = if mirror then D4.E else D4.I

		reverseIfMirror [] = []
		reverseIfMirror lst@(a : rest) =
			if mirror
				then a : List.reverse rest
				else lst

		(border : connections, crs) = TangleList.toList tangle

		substitute (i, j) =
			if i == 0
				then (i, Group.permute transform j)
				else (i, Group.permute ct j)

		crs' = map (\ (cr, g) -> (cr, ct Group.<*> g)) crs

		border' = reverseIfMirror $ b ++ a
			where
				chunk = mod (l - Dn.rotation transform) l

				(a, b) = splitAt chunk $ map substitute border

		connections' = map (reverseIfMirror . map substitute) connections


constructFromList :: (CrossingType ct) => ([[(Int, Int)]], [(ct, D4.Element)]) -> TangleSt ct
constructFromList (inc, crs)
	| n < 0            = error "constructFromList: not enough elements"
	| n /= length crs  = error "constructFromList: not enough crossing data"
	| mod l 2 /= 0     = error "constructFromList: there must be an even number of legs"
	| not ok4          = error "constructFromList: some vertex list has length different than 4"
	| otherwise        = checkConsistency `seq` result

	where
		n = (length inc) - 1
		l = length border

		border = head inc
		cross = tail inc
		ok4 = List.all (\ x -> length x == 4) cross

		makeDart (!ci, !place)
			| place < 0  = error "constructFromList: place is negative"
			| ci < 0     = error "constructFromList: index is negative"
			| otherwise  =
				if ci == 0
					then if place < l
						then LegSt result place
						else error "constructFromList: leg index is out of number of legs"
					else if ci <= n && place < 4
						then DartSt result (4 * (ci - 1) + place)
						else error "constructFromList: out of bound"

		result = TangleSt cs ds ls
			where
				cs = Array.listArray (0, n - 1) crs
				ds = Array.listArray (0, 4 * n - 1) $ map makeDart $ List.concat cross
				ls = Array.listArray (0, l - 1) $ map makeDart border

		checkConsistency = foldl (\ () d -> checkDart d `seq` ()) () (allLegsAndDarts result)
			where
				checkDart d =
					if d == ood
						then ()
						else error msg
					where
						od = opposite d
						ood = opposite od

						msg = List.concat [
							"constructFromList: inconsistency between ",
							TangleShow.showDart d,
							" and ",
							TangleShow.showDart od,
							" at ",
							show inc]


singleCrossingTangle :: (CrossingType ct) => ct -> TangleSt ct
singleCrossingTangle cr = constructFromList ([[(1, 0), (1, 1), (1, 2), (1, 3)], [(0, 0), (0, 1), (0, 2), (0, 3)]], [(cr, D4.I)])
