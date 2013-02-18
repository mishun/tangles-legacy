module Math.KnotTh.Tangles.FlypeGenerator
	(
	  generateFlypeEquivalentDecomposition
	, generateFlypeEquivalent
	) where

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Control.Monad.State.Strict as State

import Control.Monad

import qualified Math.Algebra.Group.D4 as D4
import qualified Math.KnotTh.Tangles.TangleSt as TangleSt
import qualified Math.KnotTh.Tangles.BorderIncremental.BITangle as BITangle
import qualified Math.KnotTh.Tangles.BorderIncremental.Tests as BITests
import qualified Math.KnotTh.Tangles.Util as Util

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.SubTangleCrossing
import Math.KnotTh.Crossings.ProjectionCrossing
import Math.KnotTh.Tangles.BorderIncremental.GlueSites


preTest :: (CrossingType ct) => BITangle.PreChecker ct
preTest _ _ _ = True


postTest :: (CrossingType ct) => BITangle.PostChecker ct
postTest tangle lastCrossing legsToGlue =
	BITests.postTestProjection tangle lastCrossing legsToGlue
	>>= checkSubtangles

	where
		checkSubtangles dt =
			if (legsToGlue == 3) && (not $ BITests.flowTest tangle lastCrossing)
				then Maybe.Nothing
				else Maybe.Just dt


generateFlypeEquivalentDecomposition :: (Monad m) => Int -> (BITangle.BITangle (SubTangleCrossing (TangleSt.TangleSt ProjectionCrossing)) -> m ()) -> m ()
generateFlypeEquivalentDecomposition maxN yield
	| maxN < 1   = error "generateFlypeEquivalentTempl: maxN must be at least 1"
	| otherwise  = mapM_ (\ !st -> dfs $! makeRoot st) subtangles

	where
		internalChildren !tangle !sub = Maybe.mapMaybe (BITangle.glue tangle) sites
			where
				n = numberOfCrossings tangle
				nc = numberOfSubstitutedCrossings tangle
				l = numberOfLegs tangle

				sites = filter test $ glueSites (\ _ _ -> [sub]) tangle

				test ((st, g), (gl, leg))
					| (nc + m + div l 2 - gl > maxN)  = False
					| (l == 4) && (gl == 2)           = frontOrBack && has2face && subOk
					| l == 4                          = (n == 1) && (gl == 1)
					| otherwise                       = not has2face

					where
						m = containingCrossings st

						has2face =
							let crToGlue = map adjacentCrossing $ take gl $ List.iterate nextCW leg
							in List.any (\ (a, b) -> a == b) $ zip (tail crToGlue) crToGlue

						frontOrBack =
							if n == 1
								then	let tmp = (\ (st0, g0) -> directSumDecompositionState st0 g0) $ state $ crossing 1 tangle
									in if odd $ legPosition leg
										then tmp /= DirectSum01_23
										else tmp /= DirectSum12_30
								else True

						subOk = (directSumDecompositionState st g /= DirectSum01_23)

		externalChildren !tangle !sub = Maybe.mapMaybe (BITangle.glue tangle) sites
			where
				n = numberOfCrossings tangle
				l = numberOfLegs tangle

				sites = filter test rawSites
				
				rawSites = glueSites (\ _ _ -> [sub]) tangle

				test (_, (!gl, !leg)) = (l > 2 * gl) && (not has2face) && (not connectedTo4legs)
					where
						connectedTo4legs = (l == 4) && (n > 1)

						has2face =
							let crToGlue = map adjacentCrossing $ take gl $ List.iterate nextCW leg
							in List.any (\ (a, b) -> a == b) $ zip (tail crToGlue) crToGlue


		makeRoot !subTangle = BITangle.create (preTest, postTest) subTangle

		subtangles = (\ (_, !st, _) -> Foldable.toList st) $ State.execState iterateTangles initial
			where
				loner = fromTangle 0 D4.SubD4 Loner $ TangleSt.singleCrossingTangle ProjectionCrossing

				initial = (0 :: Int, Seq.singleton loner, Seq.singleton $ makeRoot loner)

				isEnd = State.gets (\ (!tangleId, _, !tangles) -> tangleId >= Seq.length tangles)

				currentId = State.gets (\ (!tangleId, _, _) -> tangleId)

				currentTangle = State.gets (\ (!tangleId, _, !tangles) -> Seq.index tangles tangleId)

				toNextTangle = State.modify (\ (!tangleId, !subs, !tangles) -> (tangleId + 1, subs, tangles))

				getTangle tangleId = State.gets (\ (_, _, !t) -> Seq.index t tangleId)

				numberOfSubtangles = State.gets (\ (_, !subs, _) -> Seq.length subs)

				getSubtangle subId = State.gets (\ (_, !st, _) -> Seq.index st subId)

				iterateTangles = do
					end <- isEnd
					when (not end) $! do
						tangle <- currentTangle
						iterateSubTangles tangle 0
						toNextTangle
						iterateTangles

				iterateSubTangles !tangle !subId = do
					sz <- numberOfSubtangles
					when (subId < sz) $! do
						sub <- getSubtangle subId
						let m = containingCrossings sub
						when (n + m <= maxN) $ applyTangle tangle sub
						iterateSubTangles tangle (subId + 1)

					where
						n = numberOfCrossings tangle

				applyTangle !tangle !sub = when (n + m <= maxN) $ mapM_ (\ !ch -> (Util.diskHomeomorphismInvariant ch) `seq` addTangle ch) children
					where
						n = numberOfSubstitutedCrossings tangle
						m = containingCrossings sub

						children = internalChildren tangle sub

				addTangle !tangle = do
					when (l == 4) $! addSubTangle tangle
					State.modify (\ (!tangleId, !subs, !tangles) -> (tangleId, subs, tangles Seq.|> tangle))

					where
						l = numberOfLegs tangle

				addSubTangle !tangle = do
					subId <- State.gets (\ (_, !subs, _) -> Seq.length subs)
					let directSumType
						| (a == b) && (c == d)  = DirectSum01_23
						| (b == c) && (a == d)  = DirectSum12_30
						| otherwise             = NoDirectSum

						where
							[a, b, c, d] = map adjacentCrossing $ allLegs tangle

					let sub = fromTangle subId (D4.fromDnSubGroup $ BITangle.tangleSymmetry tangle) directSumType (substituteSubTangles tangle)
					State.modify (\ (!tangleId, !st, !t) -> (tangleId, st Seq.|> sub, t Seq.|> (makeRoot sub)))
					glueNewSubTangle sub 0

				glueNewSubTangle !sub !tangleId = do
					sz <- currentId
					when (tangleId < sz) $! do
						tangle <- getTangle tangleId
						applyTangle tangle sub
						glueNewSubTangle sub (tangleId + 1)

		dfs !tangle = do
			yield tangle
			when (n < maxN) $ mapM_ dfs children

			where
				n = numberOfCrossings tangle
				m = numberOfSubstitutedCrossings tangle

				subs = filter (\ !sub -> m + (containingCrossings sub) <= maxN) subtangles

				children = List.concatMap (\ !sub -> externalChildren tangle sub) subs


generateFlypeEquivalent :: (Monad m) => Int -> (TangleSt.TangleSt ProjectionCrossing -> m ()) -> m ()
generateFlypeEquivalent maxN yield = generateFlypeEquivalentDecomposition maxN (\ !templ -> yield $! substituteSubTangles templ)
