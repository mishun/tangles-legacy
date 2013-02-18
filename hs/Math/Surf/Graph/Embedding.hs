{-# LANGUAGE CPP #-}
module Math.Surf.Graph.Embedding
	(
	  stretchedCircleEmbedding
	) where

import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Control.Monad.State.Strict as State
import qualified System.IO.Unsafe as IOUnsafe

import Control.Monad
import Foreign
import Foreign.C.Types

import qualified Math.Numerical.LinearSystem as LinearSystem
import qualified Math.Numerical.GradientDescent as GradientDescent

import Math.Surf.Graph
import Math.Surf.Graph.Util


#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Math4Surf5Graph8gradientEmPKmPS3_mmmS3_S4_PKNS_9Numerical8Vector2DEPS6_"
#else
foreign import ccall "_ZN4Math4Surf5Graph8gradientEjPKjPS3_jjjS3_S4_PKNS_9Numerical8Vector2DEPS6_"
#endif
	c_gradient :: CSize-> Ptr CSize -> Ptr (Ptr CSize) -> CSize -> CSize -> CSize -> Ptr CSize -> Ptr (Ptr CSize) -> Ptr CDouble -> Ptr CDouble -> IO CDouble


gradient :: Int -> Ptr CSize -> Ptr (Ptr CSize) -> Int -> Int -> Int -> Ptr CSize -> Ptr (Ptr CSize) -> [Double] -> (Double, [Double])
gradient threadsNum threadsLen threads n v k crIncLen crInc x = IOUnsafe.unsafePerformIO $ do
	garr <- mallocArray (2 * n)
	step <- withArray (map realToFrac x) $ \ xarr ->
		c_gradient (fromIntegral threadsNum) threadsLen threads (fromIntegral n) (fromIntegral v) (fromIntegral k) crIncLen crInc xarr garr
	g <- peekArray (2 * n) garr
	free garr
	return (realToFrac step, map realToFrac g)


stretchedCircleEmbedding :: (SurfaceGraph g v f d) => g -> v -> Array.Array d [(Double, Double)]
stretchedCircleEmbedding graph start
	| eulerChar graph /= 2  = error "stretchedCircleEmbedding: can only be applied to planar graph"
	| otherwise             = simplify $ derivativeEmbedding (3 :: Int) graph start

	where
		simplify =
			if List.all (\ v -> (v == start) || (even $ numberOfVertexDarts v)) $ allVertices graph
				then relaxEmbedding graph start
				else id


derivativeEmbedding :: (Integral int, SurfaceGraph g v f d) => int -> g -> v -> Array.Array d [(Double, Double)]
derivativeEmbedding n graph start
	| n <= 0     = makeRubberConf graph start circleBorder
	| otherwise  = embedding

	where
		circleBorder = map borderPoint [0 .. (size - 1)]
			where
				size = numberOfVertexDarts start

				borderPoint i =
					let angle = -2.0 * pi * (fromIntegral i) / (fromIntegral size)
					in (cos angle, sin angle)

		der = derivedGraph graph

		derEmb = derivativeEmbedding (n - 1) der (vertex (vertexIndex start) der)

		embedding = Array.array (dartsRange graph) $ List.concatMap edgeEmbedding $ zip [(v + f) ..] (allEdges graph)
			where
				v = numberOfVertices graph
				f = numberOfFaces graph

				edgeEmbedding (i, (a, b)) = [(a, ab), (b, ba)]
					where
						edgeVertex = vertex i der

						da = vertexDart 0 edgeVertex
						db = vertexDart 2 edgeVertex

						halfA = derEmb Array.! da
						halfB = derEmb Array.! db

						ab = (List.reverse halfA) ++ (tail halfB)
						ba = (List.reverse halfB) ++ (tail halfA)


makeRubberConf :: (SurfaceGraph g v f d) => g -> v -> [(Double, Double)] -> Array.Array d [(Double, Double)]
makeRubberConf graph start border
	| not $ borderOk  = error "wrong number of elements in border"
	| otherwise       = Array.array (dartsRange graph) $ map (\ d -> (d, [dartCoords d, dartCoords $ opposite d])) $ allDarts graph

	where
		borderOk = numberOfVertexDarts start == length border

		vertexId = Array.listArray (verticesRange graph) $ scanl renum 0 $ allVertices graph
			where
				renum s v = if v == start then s else s + 1

		linearA = List.concatMap edgeSystem $ allEdges graph
			where
				edgeSystem (ad, bd)
					| aLeg && bLeg  = []
					| aLeg          = addLeg indexB
					| bLeg          = addLeg indexA
					| otherwise     = addEdge indexA indexB

					where
						a = incidentVertex ad
						b = incidentVertex bd

						aLeg = (start == a)
						bLeg = (start == b)

						indexA = vertexId Array.! a
						indexB = vertexId Array.! b

				addEdge u v = [((u, u), 1), ((v, v), 1), ((u, v), -1), ((v, u), -1)]

				addLeg u = [((u, u), 1)]

		(linearBx, linearBy) = unzip $ map makeLegXY $ zip (vertexIncidentDarts start) border
			where
				makeLegXY (d, (x, y)) = ((u, x), (u, y))
					where
						u = vertexId Array.! (incidentVertex $ opposite d)

		coords = Array.listArray (0, n - 1) $ zip (solve linearBx) (solve linearBy)
			where
				n = (numberOfVertices graph) - 1

				solve = LinearSystem.solveConjGrad n linearA

		dartCoords d =
			if start == v
				then border !! (snd $ begin d)
				else coords Array.! (vertexId Array.! v)

			where
				v = incidentVertex d


relaxEmbedding :: (SurfaceGraph g v f d) => g -> v -> Array.Array d [(Double, Double)] -> Array.Array d [(Double, Double)]
relaxEmbedding graph start initial = result
	where
		v = numberOfVertices graph
		k = numberOfVertexDarts start

		edges = allEdges graph

		(numberOfPoints, indices) = (n, indicesArr)
			where
				indicesArr = Array.array (dartsRange graph) $ List.concatMap (\ (d, l) -> [(d, l), (opposite d, List.reverse l)]) indicesAssocs

				(n, indicesAssocs) = List.mapAccumL edgeAssocs (v + k - 1) edges

				edgeAssocs offset (a, b) = (offset + l - 2, (a, ids))
					where
						l = length $ initial Array.! a

						ids = [beginIndex a] ++ [offset .. (offset + l - 3)] ++ [beginIndex b]

				beginIndex d
					| ver < start  = vertexIndex ver
					| ver > start  = vertexIndex ver - 1
					| otherwise    = (v - 1) + place

					where
						(ver, place) = begin d

		paths = map indexThread drts
			where
				indexThread path = (head $ indices Array.! (head path)) : List.concatMap (\ d -> tail $ indices Array.! d) path

				drts = fst $ State.execState (mapM_ getThread $ vertexIncidentDarts start ++ allDarts graph) ([], Set.empty)

				getThread d = do
					(t, s) <- State.get
					when (Set.notMember d s) $ do
						let thread = walkThread d
						State.put $! (thread : t, foldr Set.insert s $ thread ++ map opposite thread)
					return ()
					
				
				walkThread first = walk first []
					where
						cont d
							| odd n      = error "vertex degree must be even"
							| otherwise  = nextV (div n 2) d

							where
								n = numberOfVertexDarts $ fst $ begin d

						walk d thread =
							if incidentVertex od == start || nd == first
								then List.reverse (d : thread)
								else walk nd (d : thread)

							where
								od = opposite d

								nd = cont od

		x0 = Array.elems $ Array.array (0, numberOfPoints - 1) $ List.concatMap (\ (d, _) -> zip (indices Array.! d) (initial Array.! d)) edges

		x1 = IOUnsafe.unsafePerformIO $ do
			threadLists <- forM paths (newArray . map fromIntegral)
			crIncList <- mapM (newArray . map (\ d -> fromIntegral $ (indices Array.! d) !! 1) . vertexIncidentDarts)
				$ filter (/= start) $ allVertices graph

			res <- withArray (map List.genericLength paths) $ \ threadLengths ->
				withArray threadLists $ \ threads ->
					withArray (map (List.genericLength . vertexIncidentDarts) $ filter (/= start) $ allVertices graph) $ \ crIntLens ->
						withArray crIncList $ \ crInt -> do
							let grad x = gradient (length paths) threadLengths threads numberOfPoints (v - 1) k crIntLens crInt x
							let r = descent grad (List.concatMap (\ (x, y) -> [x, y]) x0)
							return $! Array.listArray (0, numberOfPoints - 1) $ union r

			forM_ crIncList free
			forM_ threadLists free
			return $! res

			where
				descent = GradientDescent.gradientDescent 2048 5e-3 1e-9

				union [] = []
				union [_] = []
				union (x : y : rest) = (x, y) : union rest

		result =
			let resultChain (d, ids) = (d, map ((Array.!) x1) ids)
			in Array.array (dartsRange graph) $ map resultChain $ Array.assocs indices
