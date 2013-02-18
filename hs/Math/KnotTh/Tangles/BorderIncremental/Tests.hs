module Math.KnotTh.Tangles.BorderIncremental.Tests
	(
	  projectionTest
	, projectionTestForTriangle
	, reducedProjectionTest

	, preTestTrivial
	, preTestBasicPolyhedron
	, preTestTemplate
	, preTestIrreducibleArbitrary
	, preTestIrreducibleArbitraryForTriangle
	, postTestProjection
	, postTestTemplate
	, flowTest
	) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as Unboxed
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Control.Monad.State.Strict as State

import Control.Monad

import qualified Math.Algebra.RotationDirection as RotationDirection
import qualified Math.Algebra.Group as Group
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.Algebra.Group.D4 as D4

import Math.KnotTh.Tangles.BorderIncremental.BITangle
import Math.KnotTh.Tangles.Invariants.RootCode
import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh


projectionTest :: (CrossingType ct) => (PreChecker ct, PostChecker ct)
projectionTest = (preTestTrivial, postTestProjection)


projectionTestForTriangle :: (CrossingType ct) => Int -> (PreChecker ct, PostChecker ct)
projectionTestForTriangle maxN = (preTestTriangle maxN, postTestProjection)


reducedProjectionTest :: (CrossingType ct) => (PreChecker ct, PostChecker ct)
reducedProjectionTest = (preTestBasicPolyhedron, postTestProjection)



preTestTrivial :: (CrossingType ct) => PreChecker ct
preTestTrivial tangle _ (gl, _) = let l = numberOfLegs tangle in l >= 2 * gl


preTestBasicPolyhedron :: (CrossingType ct) => PreChecker ct
preTestBasicPolyhedron tangle _ (gl, leg) = (l >= 2 * gl) && no2faces
	where
		l = numberOfLegs tangle

		crToGlue = map adjacentCrossing $ take gl $ List.iterate nextCW leg
		no2faces = List.all (\ (a, b) -> a /= b) $ zip (tail crToGlue) crToGlue


preTestTemplate :: (CrossingType ct) => PreChecker ct
preTestTemplate tangle _ (gl, leg) = (l >= 2 * gl) && (not glueTo2tangle) && no2faces
	where
		l = numberOfLegs tangle

		glueTo2tangle = (l == 4) && (numberOfCrossings tangle > 1)

		crToGlue = map adjacentCrossing $ take gl $ List.iterate nextCW leg
		no2faces = List.all (\ (a, b) -> a /= b) $ zip (tail crToGlue) crToGlue


preTestTriangle :: (CrossingType ct) => Int -> PreChecker ct
preTestTriangle maxN tangle _ (gl, _) = (l >= 2 * gl) && (n + div l 2 - gl < maxN)
	where
		n = numberOfCrossings tangle
		l = numberOfLegs tangle


preTestIrreducibleArbitrary :: BITangle ArbitraryCrossing -> (ArbitraryCrossing, D4.Element) -> (Int, BIDart ArbitraryCrossing) -> Bool
preTestIrreducibleArbitrary tangle st (gl, leg) = (l >= 2 * gl) && noReductions
	where
		l = numberOfLegs tangle

		legsToGlue = take gl $ List.iterate nextCW leg
		toTest = zip3 [(0 :: Int) ..] legsToGlue (tail legsToGlue)

		bad (i, la, lb) = (adjacentCrossing la == adjacentCrossing lb) && (passOverIndex st i) == (passOver $ opposite la)
		noReductions = List.all (not . bad) toTest


preTestIrreducibleArbitraryForTriangle :: Int -> BITangle ArbitraryCrossing -> (ArbitraryCrossing, D4.Element) -> (Int, BIDart ArbitraryCrossing) -> Bool
preTestIrreducibleArbitraryForTriangle maxN tangle st pos = (preTestTriangle maxN tangle st pos) && (preTestIrreducibleArbitrary tangle st pos)


postTestProjection :: (Tangle t c d ct) => t -> c -> Int -> Maybe.Maybe (Dn.SubGroup, (D4.Element, D4.Element))
postTestProjection tangle lastCrossing legsToGlue
	| composite  = Maybe.Nothing
	| Maybe.isNothing symmetry  = Nothing
	| otherwise  = Maybe.Just (Maybe.fromJust symmetry, basis)

	where
		(composite, cutpoints) = investigateConnectivity tangle lastCrossing legsToGlue

		symmetry = analyseSymmetry tangle lastCrossing legsToGlue ((Array.!) cutpoints)

		basis = (Maybe.fromJust $ List.find (\ g -> rc0 == rcr g) global, Maybe.fromJust $ List.find (\ g -> rc0 == rcm g) global)
			where
				period = Dn.rotationPeriod $ Maybe.fromJust symmetry
				global = Group.subGroupElements $ globalTransforms $ fst $ state lastCrossing
				rc0 = rootCodeLeg tangle (firstLeg tangle) RotationDirection.CounterClockwise D4.I
				rcr = rootCodeLeg tangle (next period $ firstLeg tangle) RotationDirection.CounterClockwise
				rcm = rootCodeLeg tangle (next (Dn.mirroredZero $ Maybe.fromJust symmetry) $ firstLeg tangle) RotationDirection.Clockwise


postTestTemplate :: (Tangle t c d ct) => t -> c -> Int -> Maybe.Maybe (Dn.SubGroup, (D4.Element, D4.Element))
postTestTemplate tangle lastCrossing legsToGlue = (postTestProjection tangle lastCrossing legsToGlue) >>= checkSubtangles
	where
		checkSubtangles dt =
			if (legsToGlue == 3) && (not $ flowTest tangle lastCrossing)
				then Maybe.Nothing
				else Maybe.Just dt



investigateConnectivity :: (Tangle t c d ct) => t -> c -> Int -> (Bool, Array.Array c Bool)
investigateConnectivity tangle lastCrossing legsToGlue = (legsToGlue == 3 && borderCut < 3, cutpointsMask Array.// [(lastCrossing, False)])
	where
		((_, borderCut), (_, _, cutpointsMask)) = State.runState (dfs lastCrossing lastCrossing) initial
			where
				initial = (1 :: Int, zeroes, falses)
				falses = Unboxed.listArray (crossingsRange tangle) $ List.repeat False
				zeroes = Unboxed.listArray (crossingsRange tangle) $ List.repeat (0 :: Int)


		tinFromState !v (_, !tin, _) = tin Array.! v
		colorNew !v (!timer, !tin, !cp) = (timer + 1, tin Array.// [(v, timer)], cp)
		markCutpoint !v (!timer, !tin, !cp) = (timer, tin, cp Array.// [(v, True)])

		dfs v from = do
			State.modify $ colorNew v
			tin <- State.gets $ tinFromState v
			res <- foldM processEdge (tin, 0 :: Int) neigh
			return res

			where
				neigh = filter (\ !d -> (isLeg d) || ((fst $ begin d) /= from)) $ map opposite $ incidentDarts v

				processEdge (fup, border) e
					| isLeg e   = return (fup, border + 1)
					| otherwise = do
						uTin <- State.gets $ tinFromState u
						if uTin > 0
							then return (min fup uTin, border)
							else do
								(thatFup, thatBorder) <- dfs u v
								vTin <- State.gets $ tinFromState v
								if thatFup >= vTin
									then State.modify $ markCutpoint v
									else return ()
								return (min fup thatFup, border + if thatFup <= vTin then thatBorder else 1)

					where
						u = fst $ begin e


analyseSymmetry :: (Tangle t c d ct) => t -> c -> Int -> (c -> Bool) -> Maybe.Maybe Dn.SubGroup
analyseSymmetry tangle lastCrossing legsToGlue skipCrossing = findSymmetry
	where
		global = Group.subGroupElements $ globalTransforms $ fst $ state $ lastCrossing

		lastRootCode = min rcCCW rcCW
			where
				startCCW = fst $ legsRange tangle
				startCW = (List.iterate nextCCW $ startCCW) !! (3 - legsToGlue)

				rcCCW = List.minimum $ map (rootCodeLeg tangle startCCW RotationDirection.CounterClockwise) global
				rcCW = List.minimum $ map (rootCodeLeg tangle startCW RotationDirection.Clockwise) global


		findSymmetry = case finalState of
			Nothing -> Nothing
			Just (symmetryDir, symmetryRev, positionDir, positionRev) ->
				let
					l = numberOfLegs tangle
					period = div l (max symmetryDir symmetryRev)

				in if symmetryDir == symmetryRev
					then Just $ Dn.fromPeriodAndMirroredZero l period (positionRev + positionDir)
					else Just $ Dn.fromPeriod l period

			where
				finalState = foldM analyseLeg (z, z, z, z) numberedLegs
					where
						z = 0 :: Int
						numberedLegs = zip (allLegs tangle) [z .. ]


				testCW (leg, legIndex) st@(symDir, symRev, posDir, _) = case compare curCode lastRootCode of
					LT -> Nothing
					GT -> Just st
					EQ -> Just (symDir, symRev + 1, posDir, legIndex)

					where
						curCode = List.minimum $ map (rootCodeLeg tangle leg RotationDirection.Clockwise) global

				testCCW (leg, legIndex) st@(symDir, symRev, _, posRev) = case compare curCode lastRootCode of
					LT -> Nothing
					GT -> Just st
					EQ -> Just (symDir + 1, symRev, legIndex, posRev)

					where
						curCode = List.minimum $ map (rootCodeLeg tangle leg RotationDirection.CounterClockwise) global

				analyseLeg state0 (leg, legIndex) =
					if skipCrossing cur
						then Just state0
						else do
							state1 <- if nx /= cur then testCW (leg, legIndex) state0 else return state0
							state2 <- if pv /= cur then testCCW (leg, legIndex) state1 else return state1
							Maybe.Just state2

					where
						cur = fst $ begin $ opposite leg
						nx = fst $ begin $ opposite $ nextCCW leg
						pv = fst $ begin $ opposite $ nextCW leg


flowTest :: (Tangle t c d ct) => t -> c -> Bool
flowTest tangle finish = State.evalState flowOk (Array.listArray (dartsRange tangle) $ List.repeat (0 :: Int))
	where
		adjFlow list = State.modify (\ f -> f Array.// (map (\ (d, df) -> (d, df + (f Array.! d))) list))

		flowOk = do
			trivialFlow
			p <- fordFulkerson
			total <- State.gets (\ f -> -(List.sum $ map ((Array.!) f) $ incidentDarts finish))
			if total == 4
				then return $ List.all (\ d -> Map.member (incidentCrossing d) p) $ filter (not . isLeg) $ adjacentDarts finish
				else return False

		trivialFlow = do
			let d = filter (isLeg . opposite) $ incidentDarts finish
			adjFlow $ zip d (List.repeat (-1))

		fordFulkerson = do
			flow <- State.get
			let p = bfs flow
			if Map.member finish p
				then do
					let path c
						| isLeg pc   = [(cp, -1)]
						| otherwise  = (cp, -1) : (pc, 1) : path (incidentCrossing pc)
						where
							cp = p Map.! c
							pc = opposite cp

					adjFlow $ path finish
					fordFulkerson
				else return p

		bfs flow = State.evalState runBfs initial
			where
				initial = List.foldl' process (Seq.empty, Map.empty) (allLegs tangle)
					where
						bigLeg =
							if numberOfLegs tangle == 4
								then Maybe.Just $ next (2 :: Int) $ firstLeg tangle
								else Maybe.Nothing

						process (q, p) l =
							if (Map.member c p) || ((Maybe.Just l /= bigLeg) && flow Array.! d /= 0)
								then (q, p)
								else (q Seq.|> c, Map.insert c d p)

							where
								d = opposite l
								c = incidentCrossing d

				isEmpty = State.gets (\ (q, _) -> Seq.null q)

				enqueue c d = State.modify $ (\ (q, p) -> (q Seq.|> c, Map.insert c d p))

				dequeue = do
					(c Seq.:< rest) <- State.gets (\ (q, _) -> Seq.viewl q)
					State.modify (\ (_, p) -> (rest, p))
					return c

				isVisited c = State.gets (\ (_, p) -> Map.member c p)

				result = State.gets (\ (_, p) -> p)

				runBfs = do
					empty <- isEmpty
					if empty
						then result
						else do
							u <- dequeue
							if u == finish
								then result
								else do
									mapM_ relax (filter (\ d -> flow Array.! d > -1) $ filter (not . isLeg) $ adjacentDarts u)
									runBfs

					where
						relax d =
							let v = incidentCrossing d
							in do
								vis <- isVisited v
								when (not vis) $ enqueue v d
