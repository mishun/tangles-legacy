module Math.Surf.Graph
	(
	  SurfaceGraph(..)
	, end
	, right
	, toPair
	, nextV
	, nextF
	, allVertices
	, allFaces
	, allDarts
	, allEdges
	, vertexIncidentDarts
	, faceIncidentDarts
	, adjacentVertices
	, incidentVertex
	, eulerChar
	, genus
	, isTriangulation

	) where

import qualified Data.Ix as Ix
import qualified Data.List as List


class (Ix.Ix vertex, Ix.Ix face, Ix.Ix dart) => SurfaceGraph graph vertex face dart
		| graph -> vertex
		, vertex -> face
		, face -> dart
		, dart -> graph
	where
		rotateCW, rotateCCW :: dart -> dart
		walkCW, walkCCW     :: dart -> dart
		opposite            :: dart -> dart
		begin               :: dart -> (vertex, Int)
		left                :: dart -> (face, Int)

		numberOfVertices, numberOfEdges, numberOfFaces :: graph -> Int

		verticesRange :: graph -> (vertex, vertex)
		facesRange    :: graph -> (face, face)
		dartsRange    :: graph -> (dart, dart)

		numberOfVertexDarts :: vertex -> Int
		numberOfFaceDarts   :: face -> Int

		vertexIndex :: vertex -> Int
		faceIndex   :: face -> Int
		dartIndex   :: dart -> Int

		vertexDart :: Int -> vertex -> dart
		faceDart   :: Int -> face -> dart

		vertex :: Int -> graph -> vertex
		face   :: Int -> graph -> face


end :: (SurfaceGraph g v f d) => d -> (v, Int)
end = begin . opposite


right :: (SurfaceGraph g v f d) => d -> (f, Int)
right = left . opposite


toPair :: (SurfaceGraph g v f d) => d -> (Int, Int)
toPair d = (vertexIndex v, place)
	where
		(v, place) = begin d


nextV :: (Integral int, SurfaceGraph g v f d) => int -> d -> d
nextV steps d =
	if steps >= 0
		then List.genericIndex (List.iterate rotateCCW d) steps
		else List.genericIndex (List.iterate rotateCW d) (-steps)


nextF :: (Integral int, SurfaceGraph g v f d) => int -> d -> d
nextF steps d =
	if steps >= 0
		then List.genericIndex (List.iterate walkCCW d) steps
		else List.genericIndex (List.iterate walkCW d) (-steps)


allVertices :: (SurfaceGraph g v f d) => g -> [v]
allVertices = Ix.range . verticesRange


allFaces :: (SurfaceGraph g v f d) => g -> [f]
allFaces = Ix.range . facesRange


allDarts :: (SurfaceGraph g v f d) => g -> [d]
allDarts = Ix.range . dartsRange


allEdges :: (SurfaceGraph g v f d) => g -> [(d, d)]
allEdges = filter (\ (a, b) -> a < b) . map (\ d -> (d, opposite d)) . allDarts


vertexIncidentDarts :: (SurfaceGraph g v f d) => v -> [d]
vertexIncidentDarts v =
	let n = numberOfVertexDarts v
	in map (\ i -> vertexDart i v) [0 .. (n - 1)]


faceIncidentDarts :: (SurfaceGraph g v f d) => f -> [d]
faceIncidentDarts f =
	let n = numberOfFaceDarts f
	in map (\ i -> faceDart i f) [0 .. (n - 1)]


adjacentVertices :: (SurfaceGraph g v f d) => v -> [v]
adjacentVertices = map (fst . begin . opposite) . vertexIncidentDarts


incidentVertex :: (SurfaceGraph g v f d) => d -> v
incidentVertex = fst . begin


eulerChar :: (SurfaceGraph g v f d) => g -> Int
eulerChar graph = v + f - e
	where
		v = numberOfVertices graph
		f = numberOfFaces graph
		e = numberOfEdges graph


genus :: (SurfaceGraph g v f d) => g -> Int
genus graph = div (2 - eulerChar graph) 2


isTriangulation :: (SurfaceGraph g v f d) => g -> Bool
isTriangulation = List.all (\ f -> 3 == numberOfFaceDarts f) . allFaces
