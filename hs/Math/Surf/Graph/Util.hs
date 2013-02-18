module Math.Surf.Graph.Util
	(
	  dualGraph
	, derivedGraph
	, nthDerivedGraph
	, sphereStarDecomposition
	) where

import qualified Data.List as List
import qualified Data.Array as Array

import Math.Surf.Graph
import Math.Surf.Graph.GraphSt
import Math.Surf.Graph.Construction


dualGraph :: (SurfaceGraph g v f d) => g -> GraphSt
dualGraph = constructFromList . (map faceAdjList) . allFaces
	where
		faceAdjList = map (toCell . right) . faceIncidentDarts
			where
				toCell (fc, place) = (faceIndex fc, place)


--      v1 (2)          e2 (4)                                 \ v2 (4)
--        *               |     f1 (3)                          *
--        |               |                                    /
-- f1 (3) | f0 (1)      --*---- e1 (2)                        / e1 (3)
--        |               |                     \            /
--        *               |     f0 (1)           \  e0 (1)  /
--      v0 (0)          e0 (0)             v0 (0) *--------* v1 (2)
derivedGraph :: (SurfaceGraph g v f d) => g -> GraphSt
derivedGraph graph = constructFromList (vertexPart ++ facePart ++ edgePart)
	where
		v = numberOfVertices graph
		f = numberOfFaces graph

		edges = allEdges graph

		newFaceIndex fc = v + faceIndex fc

		edgeIndexLookup = Array.array (dartsRange graph) $ List.concat $ map indexEdge $ zip edges [(v + f) ..]
			where
				indexEdge ((a, b), eId) = [(a, eId), (b, eId)]

		vertexPart = map make $ allVertices graph
			where
				vertexToEdge d = ((Array.!) edgeIndexLookup d, if d < opposite d then 0 else 2)

				vertexToFace d = (newFaceIndex fc, 2 * place)
					where
						(fc, place) = left d

				make = List.concatMap (\ d -> [vertexToEdge d, vertexToFace d]) . vertexIncidentDarts

		facePart = map make $ allFaces graph
			where
				faceToEdge d = ((Array.!) edgeIndexLookup d, if d < opposite d then 3 else 1)

				faceToVertex d = (vertexIndex ver, 2 * place + 1)
					where
						(ver, place) = begin d

				make = List.concatMap (\ d -> [faceToVertex d, faceToEdge d]) . faceIncidentDarts

		edgePart = map make edges
			where
				edgeToVertex d = (vertexIndex ver, 2 * place)
					where
						(ver, place) = begin d

				edgeToFace d = (newFaceIndex fc, 2 * place + 1)
					where
						(fc, place) = left d

				make (b, e) = [edgeToVertex b, edgeToFace e, edgeToVertex e, edgeToFace b]


nthDerivedGraph :: (Integral int, SurfaceGraph g v f d) => int -> g -> GraphSt
nthDerivedGraph n graph
	| n <= 0     = clone graph
	| n == 1     = derivedGraph graph
	| otherwise  = derivedGraph $ nthDerivedGraph (n - 1) graph


sphereStarDecomposition :: (SurfaceGraph g v f d) => g -> (GraphSt, GraphSt)
sphereStarDecomposition graph
	| genus graph == 0  = error "sphereStarDecomposition has no sense for genus 0"
	| otherwise         = (clone graph, clone graph) -- TODO: implement
