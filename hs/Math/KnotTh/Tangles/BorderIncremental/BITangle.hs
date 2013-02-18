module Math.KnotTh.Tangles.BorderIncremental.BITangle
	(
	  BITangle
	, BICrossing
	, BIDart

	, PreChecker
	, PostChecker

	, tangleSymmetry
	, globalTransformBasis
	, ancestor
	, create
	, glue
	) where

import qualified Data.Ix as Ix
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Bits as Bits
import qualified Data.Array as Array

import qualified Math.Algebra.Group as Group
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.Algebra.Group.D4 as D4
import qualified Math.KnotTh.Tangles.Util.Show as TangleShow

import Math.KnotTh.Tangles
import Math.KnotTh
import Math.KnotTh.Tangles.TangleSkeleton
import Math.KnotTh.Tangles.BorderIncremental.BITangleSkeleton


data BIDart ct = Dart
	{
		_dartOnwer :: BITangle ct,
		_dartIndex :: !DartSkeleton
	}

data BICrossing ct = Crossing
	{
		_crossingOwner :: BITangle ct,
		_crossingIndex :: !CrossingSkeleton
	}

type PreChecker ct = (BITangle ct -> (ct, D4.Element) -> (Int, BIDart ct) -> Bool)
type PostChecker ct = (BITangle ct -> BICrossing ct -> Int -> Maybe (Dn.SubGroup, (D4.Element, D4.Element)))

data BITangle ct = BorderIncrementalTangle
	{
		_incrementalCheck :: !(PreChecker ct, PostChecker ct),
		_skeleton :: !TangleSkeleton,
		_crossings :: !(Array.Array Int (ct, D4.Element)),
		_symmetry :: (Dn.SubGroup, (D4.Element, D4.Element)),
		_ancestor :: Maybe.Maybe (BITangle ct)
	}


instance (CrossingType ct) => Eq (BIDart ct) where
	(==) (Dart _ (DartSkeleton id1)) (Dart _ (DartSkeleton id2)) = (id1 == id2)

instance (CrossingType ct) => Eq (BICrossing ct) where
	(==) (Crossing _ (CrossingSkeleton id1)) (Crossing _ (CrossingSkeleton id2)) = (id1 == id2)


instance (CrossingType ct) => Ord (BIDart ct) where
	compare (Dart _ (DartSkeleton id1)) (Dart _ (DartSkeleton id2)) = compare id1 id2

instance (CrossingType ct) => Ord (BICrossing ct) where
	compare a b = compare ai bi
		where
			(CrossingSkeleton ai) = _crossingIndex a
			(CrossingSkeleton bi) = _crossingIndex b


instance (CrossingType ct) => Ix.Ix (BIDart ct) where
--	range (a@(Dart t i), b@(Dart _ j))
--		| isLeg a && isLeg b    = [Dart t k | k <- [i .. j]]
--		| isDart a && isDart b  = [Dart t (k Bits..&. 3) ]
--		| otherwise             = error "range from leg and dart"

--	range (Dart o id1, Dart _ id2) = map (\ i -> Dart o i) [id1 .. id2]
--	range (Leg o id1, Leg _ id2)   = map (\ i -> Leg o i) [id1 .. id2]
--	range _ = error "range from leg and dart"

--	index (Dart _ id1, Dart _ _) (Dart _ id0) = id0 - id1
--	index (Leg _ id1, Leg _ _) (Leg _ id0) = id0 - id1
--	index _ _ = error "index from leg and dart"

--	inRange (Dart _ id1, Dart _ id2) (Dart _ id0) = (id0 >= id1) && (id0 <= id2)
--	inRange (Leg _ id1, Leg _ id2) (Leg _ id0) = (id0 >= id1) && (id0 <= id2)
--	inRange _ _ = error "inRange from leg and dart"

--	rangeSize (Dart _ id1, Dart _ id2) = max 0 (1 + id2 - id1)
--	rangeSize (Leg _ id1, Leg _ id2) = max 0 (1 + id2 - id1)
--	rangeSize _ = error "rangeSize from leg and dart"

instance (CrossingType ct) => Ix.Ix (BICrossing ct) where
	range (Crossing t (CrossingSkeleton i), Crossing _ (CrossingSkeleton j)) =
		if i <= j
			then [Crossing t (CrossingSkeleton k) | k <- [i .. j]]
			else []

	index (Crossing _ (CrossingSkeleton i), Crossing _ (CrossingSkeleton j)) (Crossing _ (CrossingSkeleton k)) =
		if (i <= k) && (k <= j)
			then fromIntegral (k - i)
			else error "crossing is not in range"

	inRange (Crossing _ (CrossingSkeleton i), Crossing _ (CrossingSkeleton j)) (Crossing _ (CrossingSkeleton k)) = (i <= k) && (k <= j)

	rangeSize (Crossing _ (CrossingSkeleton i), Crossing _ (CrossingSkeleton j)) =
		max 0 ((fromIntegral j) - (fromIntegral i) + 1)


instance (CrossingType ct) => Knotted (BITangle ct) (BICrossing ct) (BIDart ct) ct where

	nextCCW (Dart t i) = Dart t $! skeletonNextCCW (_skeleton t) i
	nextCW (Dart t i) = Dart t $! skeletonNextCW (_skeleton t) i
	opposite (Dart t i) = Dart t $! skeletonOpposite (_skeleton t) i

	begin (Dart t (DartSkeleton i)) =
		let ci = Bits.shiftR i 8
		in if ci == 0
			then error "begin: from leg"
			else (Crossing t $! CrossingSkeleton (fromIntegral ci), fromIntegral $! i Bits..&. 3)

	index (Crossing _ (CrossingSkeleton i)) = fromIntegral i

	state (Crossing t (CrossingSkeleton i)) = (_crossings t) Array.! (fromIntegral i)

	numberOfCrossings t = (snd $ Array.bounds $ _crossings t)

	crossingsRange t =
		let n = numberOfCrossings t
		in (Crossing t $ CrossingSkeleton 1, Crossing t $! CrossingSkeleton $ fromIntegral n)

	dartsRange t =
		let n = numberOfCrossings t
		in (Dart t $ DartSkeleton 256, Dart t $ DartSkeleton $ 256 * (fromIntegral n) + 3)

	crossing i t =
		if i < 1 || i > (numberOfCrossings t)
			then error "crossing: index is out of bound"
			else Crossing t $! CrossingSkeleton $! fromIntegral i

	dart d (Crossing t (CrossingSkeleton i)) = Dart t $! DartSkeleton (256 * i + fromIntegral (d Bits..&. 3))


instance (CrossingType ct) => Tangle (BITangle ct) (BICrossing ct) (BIDart ct) ct where

	isLeg (Dart _ (DartSkeleton i)) = i < 256

	isDart = not . isLeg

	legPosition (Dart _ (DartSkeleton i)) =
		if i < 256
			then fromIntegral i
			else error "legPosition: from dart"

	numberOfLegs t = skeletonNumberOfLegs (_skeleton t)

	legsRange t =
		let l = numberOfLegs t
		in (Dart t (DartSkeleton 0), Dart t (DartSkeleton $! fromIntegral (l - 1)))


instance (CrossingType ct, Show ct) => Show (BITangle ct) where
	show = TangleShow.showTangleStructure


tangleSymmetry :: (CrossingType ct) => BITangle ct -> Dn.SubGroup
tangleSymmetry = fst . _symmetry


globalTransformBasis :: (CrossingType ct) => BITangle ct -> (D4.Element, D4.Element)
globalTransformBasis = snd . _symmetry


ancestor :: (CrossingType ct) => BITangle ct -> Maybe.Maybe (BITangle ct)
ancestor = _ancestor


create :: (CrossingType ct) => (PreChecker ct, PostChecker ct) -> ct -> BITangle ct
create test crossingType = tangle
	where
		local = localSymmetry crossingType

		symmetry = D4.toDnSubGroup $ Group.unionSubGroups local (globalTransforms crossingType)

		tangle =
			let cs = Array.listArray (1, 1) [ (crossingType, D4.I) ]
			in BorderIncrementalTangle test skeletonCreateLoner cs (symmetry, basis) Maybe.Nothing

		basis = (Maybe.fromJust $ List.find (\ g -> z == r g) global, Maybe.fromJust $ List.find (\ g -> z == m g) global)
			where
				global = Group.subGroupElements $ globalTransforms crossingType

				z = Group.factorSetId local D4.I
				r g = Group.factorSetId local ((D4.fromDnElement $ Dn.rotationBasis symmetry) Group.<*> g)
				m g = Group.factorSetId local ((D4.fromDnElement $ Dn.reflectionBasis symmetry) Group.<*> g)


--       edgesToGlue = 1                 edgesToGlue = 2                 edgesToGlue = 3
-- ........|                       ........|                       ........|
-- (leg+1)-|---------------3       (leg+1)-|---------------2       (leg+1)-|---------------1
--         |  +=========+                  |  +=========+                  |  +=========+
--  (leg)--|--|-0-\ /-3-|--2        (leg)--|--|-0-\ /-3-|--1        (leg)--|--|-0-\ /-3-|--0
-- ........|  |    *    |                  |  |    *    |                  |  |    *    |
-- ........|  |   / \-2-|--1       (leg-1)-|--|-1-/ \-2-|--0       (leg-1)-|--|-1-/ \   |
-- ........|  |  1      |          ........|  +=========+                  |  |      2  |
-- ........|  |   \-----|--0       ........|                       (leg-2)-|--|-----/   |
-- ........|  +=========+          ........|                       ........|  +=========+
glue :: (CrossingType ct) => BITangle ct -> ((ct, D4.Element), (Int, BIDart ct)) -> Maybe.Maybe (BITangle ct)
glue !tangle (!newCrossingState, gluePosition@(!legsToGlue, !leg))
	| (legsToGlue < 1 || legsToGlue > 3)  = error "legsToGlue must be in range [1..3]"
	| not $ isLeg leg  = error "leg expected"
	| not $ preChecker tangle newCrossingState gluePosition  = Nothing
	| not $ Maybe.isJust symmetry  = Nothing
	| otherwise  = Maybe.Just result 

	where
		checker@(preChecker, postChecker) = _incrementalCheck tangle

		n = numberOfCrossings tangle

		symmetry = postChecker result (Crossing result (CrossingSkeleton $! fromIntegral $! n + 1)) legsToGlue

		skeleton = skeletonGlue (_skeleton tangle) legsToGlue (_dartIndex leg)

		cs = Array.listArray (1, n + 1) $ (map state $ allCrossings tangle) ++ [ newCrossingState ]

		result = BorderIncrementalTangle checker skeleton cs (Maybe.fromJust symmetry) (Maybe.Just tangle)
