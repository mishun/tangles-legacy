module Math.KnotTh.Tangles.Util.Embedding
	(
	  tangleEmbedding
	) where

import qualified Data.List as List
import qualified Data.Array as Array

import qualified Math.KnotTh.Tangles.Util.List as TangleList
import qualified Math.Surf.Graph as Graph
import qualified Math.Surf.Graph.Embedding as GraphEmbedding

import Math.KnotTh
import Math.KnotTh.Tangles


tangleEmbedding :: (Tangle t c d ct) => t -> (Array.Array d [(Double, Double)], Array.Array d [(Double, Double)], Array.Array c (Double, Double))
tangleEmbedding tangle = (legsArray, dartsArray, crossingCoords)
	where
		graph = TangleList.toGraph tangle
		start = Graph.vertex 0 graph

		graphEmbedding = GraphEmbedding.stretchedCircleEmbedding graph (Graph.vertex 0 graph)

		(gLegs, gDarts) = List.partition (\ (d, _) -> start == Graph.incidentVertex d) $ Array.assocs graphEmbedding

		legAssocs = map assoc gLegs
			where
				l = numberOfLegs tangle

				assoc (gl, a) =
					let (_, place) = Graph.begin gl
					in ((allLegs tangle) !! (mod (l - place) l), a)

		dartAssocs = map assoc gDarts
			where
				assoc (gd, a) = (dart place (crossing verId tangle), a)
					where
						(ver, place) = Graph.begin gd
						verId = Graph.vertexIndex ver

		legsArray = Array.array (legsRange tangle) legAssocs

		dartsArray = Array.array (dartsRange tangle) dartAssocs

		crossingCoords = Array.array (crossingsRange tangle) $ map coords $ allCrossings tangle
			where
				coords c = (c, head $ dartsArray Array.! (dart 0 c))
