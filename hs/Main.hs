module Main
	(
	  main
	) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Control.Monad.State.Strict as State
import qualified System.CPUTime as CPUTime

import Math.KnotTh
import Math.KnotTh.Tangles
import qualified Math.KnotTh.Tangles.Util.Draw as TangleDraw
import qualified Math.KnotTh.Tangles.ArbitraryGenerator as ArbG
import qualified Math.KnotTh.Tangles.FlypeGenerator as AltG
import qualified Math.KnotTh.Crossings.SubTangleCrossing as SubT
import qualified Math.KnotTh.Crossings.ArbitraryCrossing as ArbC
import qualified Math.KnotTh.Crossings.ProjectionCrossing as ProjC
import qualified Math.KnotTh.Tangles.BorderIncremental.Generator as G
import qualified Math.KnotTh.Tangles.BorderIncremental.Tests as Tst
import qualified Math.KnotTh.Tangles.Util.Alternating as Alt
import Graph
import Control.Monad
import Math.KnotTh.Tangles.Invariants.InvariantsSet


main :: IO ()
main = do
	beginTime <- CPUTime.getCPUTime
	printTable
	--writePostScriptFile "output.ps" $ transformed [shifted (30, 700), scaled 20] drawTangles
	--putStrLn $ toMetaPost $ transformed [scaled 15] drawTangles
	--testJones
	endTime <- CPUTime.getCPUTime
	putStrLn $ (show $ ((fromInteger (endTime - beginTime)) :: Float) / 1.0e12)
	return ()


drawTangles :: Image ()
drawTangles = do
	forM_ (zip result offsets) $ \ (tangles, (x0, y0)) -> do
		let lineCap = 10
		let coords = [ (i, j) | j <- [0 ..], i <- [0 .. (lineCap - 1)]]

		forM_ (zip tangles coords) $ \ (tangle, (x, y)) -> do
			transformed [shifted ((x0 + x) * 2.5, (y0 + y) * (-2.5))] $ TangleDraw.drawTangle tangle
			return ()

	where
		offsets = [(0, 0), (0, 1), (0, 2), (5, 0), (0, 3), (5, 1)]

		result = Map.elems $ State.execState (ArbG.generateArbitrary 9 yield) Map.empty
			where
				yield tangle = when (numberOfCrossings tangle <= 7) $ do
					let n = numberOfCrossings tangle
					let l = numberOfLegs tangle
					let update !k = Maybe.Just $! Maybe.maybe [tangle] (\ lst -> lst ++ [tangle]) k
					State.get >>= (\ !m -> n `seq` l `seq` State.put $! Map.alter update (n, l) m)
					return ()

{-drawDuplicates :: IO ()
drawDuplicates = do
	writePostScriptFile "output.ps" $ transformed [shifted (30, 20), scaled 10] $ do
		forM_ dup $ \ t -> do
			TangleDraw.drawTangle t
			transformed [shifted (3, 0)] $ TangleDraw.drawTangle $ Maybe.fromJust $ ArbG.extractDiagram $ EqCl.find (diskHomeomorphismInvariant t) cl
			appendTransform [shifted (0, 2.5)]

	forM_ dup $ \ t -> do
		print t
		putStrLn ""
		print $ Maybe.fromJust $ ArbG.extractDiagram $ EqCl.find (diskHomeomorphismInvariant t) cl
		putStrLn ""
		let inv = diskHomeomorphismInvariant t
		forM_ (EqCl.keys cl) $ \ k -> when (EqCl.sameClass cl k inv) $ print k
		putStrLn "\n\n"

	return ()

	where
		n = 9

		(cl, set) = State.runState (ArbG.generateArbitrary (n + 1) yield) Set.empty
			where
				yield tangle = State.get >>= (\ !s -> State.put $! Set.insert (diskHomeomorphismInvariant tangle) s)

		tangles = State.execState (ArbG.generateArbitrary n yield) []
			where
				yield tangle = State.get >>= (\ !s -> State.put $! tangle : s)

		dup = filter (\ t -> Set.notMember (diskHomeomorphismInvariant t) set) tangles
-}

testJones :: IO ()
testJones = do
	forM_ byJones $ \ (jones, tangles) -> do
		when (length tangles > 1) $ do
			print "collision found"

	writePostScriptFile "output.ps" $ transformed [shifted (30, 20), scaled 10] $ do
		forM_ (zip (filter (\ (_, lst) -> length lst > 1) byJones) (List.concatMap (\ i -> [(0, i), (10, i)]) [0 ..])) $ \ ((_, lst), (dx, dy)) -> do
			forM_ (zip [0 ..] lst) (\ (i, tangle) -> transformed [shifted (2.5 * (dx + i), 2.5 * dy)] $ TangleDraw.drawTangle tangle)

	where
		byJones = Map.assocs $ State.execState (ArbG.generateArbitrary 3 yieldTangle) Map.empty
			where
				yieldTangle tangle = do
					let update !ml = case ml of
						Maybe.Nothing  -> return $! [tangle]
						Maybe.Just lst -> return $! tangle : lst

					let j = invariantsSet tangle

					State.get >>= (\ !m -> j `seq` State.put $! Map.alter update j m)


printTable :: IO ()
printTable = putStrLn tableBody
	where
		maxN = 8

		possibleLegs = [ 2 * (i + 1) | i <- [1 .. maxN] ]

		table = State.execState (generator yield) Map.empty
			where
				--generator = ArbG.generateArbitrary maxN
				generator = AltG.generateFlypeEquivalent maxN
				--generator = G.generateFromCrossings (Tst.preTestTemplate, Tst.postTestTemplate) [ProjC.ProjectionCrossing] maxN
				--generator = G.generateFromCrossings (Tst.preTestTrivial, Tst.postTestProjection) [ProjC.ProjectionCrossing] maxN
				--generator = G.generateFromCrossings (Tst.preTestIrreducibleArbitrary, Tst.postTestProjection) [ArbC.ArbitraryCrossing] maxN

				yield tangle = do
					let n = numberOfCrossings tangle
					let l = numberOfLegs tangle
					let update !k = Maybe.Just $! Maybe.maybe (1 :: Int) (+ 1) k
					State.get >>= (\ !m -> n `seq` l `seq` State.put $! Map.alter update (n, l) m)

		tableBody = List.intercalate "\n" $ [header] ++ (map line possibleLegs) ++ [allCr, total]
			where
				total = "total: " ++ (show $ List.sum $ map snd $ Map.assocs table)

				header = List.intercalate "\t" $ "l\\n" : (map show [1 .. maxN])

				line l = List.intercalate "\t" $ (show l) : (map (\ c -> cell (c, l)) [1 .. maxN])

				allCr = List.intercalate "\t" $ "all:" : (map (show . numForCr) [1 .. maxN])
					where
						numForCr c = List.foldl' (\ !carry ((!cc, !_), !cn) -> carry + if cc == c then cn else 0) 0 $ Map.assocs table

				cell arg =
					if Maybe.isJust r && x > 0
						then show x
						else "."

					where
						r = Map.lookup arg table
						x = Maybe.fromJust r
