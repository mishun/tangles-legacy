module Math.Surf.Graph.Construction
	(
	  clone
	, construct
	, constructFromList
	) where

import qualified Data.Ix as Ix
import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Array.IArray as IArray
import qualified Data.Set as Set

import Math.Surf.Graph
import Math.Surf.Graph.GraphSt
import Math.Surf.Graph.Dart
import Math.Surf.Graph.Vertex
import Math.Surf.Graph.Face


clone :: (SurfaceGraph g v f d) => g -> GraphSt
clone graph = result
	where
		v = numberOfVertices graph
		f = numberOfFaces graph
		e = numberOfEdges graph

		cloneDart drt = Dart result $ dartIndex drt
		cloneVertex vert = Vertex result $ vertexIndex vert
		cloneFace fc = Face result $ faceIndex fc

		dartsArr = Array.listArray (0, 2 * e - 1) dartsList
			where
				dartsList = map makeDartInfo $ Ix.range $ dartsRange graph

				makeDartInfo d = (cloneDart opp, (cloneVertex ver, verPlace), (cloneFace fc, fcPlace))
					where
						opp = opposite d
						(ver, verPlace) = begin d
						(fc, fcPlace) = left d

		verticesArr = Array.listArray (0, v - 1) verticesList
			where
				verticesList = map makeVertexInfo $ Ix.range $ verticesRange graph

				makeVertexInfo ver = Array.listArray (0, n - 1) $ map cloneDart $ vertexIncidentDarts ver
					where
						n = numberOfVertexDarts ver

		facesArr = Array.listArray (0, f - 1) facesList
			where
				facesList = map makeFaceInfo $ Ix.range $ facesRange graph

				makeFaceInfo fc = Array.listArray (0, n - 1) $ map cloneDart $ faceIncidentDarts fc
					where
						n = numberOfFaceDarts fc

		result = GraphSt dartsArr verticesArr facesArr


wrapToArray :: [a] -> Array.Array Int a
wrapToArray list = Array.listArray (0, (length list) - 1) list


wrapToArray2 :: [[a]] -> Array.Array Int (Array.Array Int a)
wrapToArray2 = wrapToArray . (map wrapToArray)


construct :: (IArray.IArray vertArr (adjArr Int (Int, Int)), IArray.IArray adjArr (Int, Int))
	=> vertArr Int (adjArr Int (Int, Int)) -> GraphSt

construct graph
	| (fst $ IArray.bounds graph) /= 0  = error "construct: vertex numbering must start from zero"
	| v <= 0                            = error "construct: list must contain positive number of elements"
	| not ok                            = error "construct: bad connections in adjacency list"
	| otherwise                         = result

	where
		v = 1 + (snd $ IArray.bounds graph)

		size = Array.listArray (0, v - 1) $ map getSize $ IArray.elems graph
			where
				getSize arr
					| l /= 0     = error "construct: adjacence list numbering must start from zero"
					| r < 0      = error "construct: adjacence list must contain at least one element"
					| otherwise  = r + 1

					where
						(l, r) = IArray.bounds arr

		adjacentSize i = (IArray.!) size i

		adjacentCell (i, j) = (IArray.!) ((IArray.!) graph i) j

		rotatedCellCW (i, j) =
			let n = adjacentSize i
			in (i, mod (j + n - 1) n)

		ok = List.all checkVertex [0 .. (v - 1)]
			where
				checkVertex i = List.all checkCell [0 .. (n - 1)]
					where
						n = adjacentSize i

						checkCell j
							| x < 0 || x >= v || y < 0 || y >= m  = error "construct: out of bound"
							| (x, y) == (i, j)                    = error "construct: connection to itself"
							| otherwise                           = back == (i, j)

							where
								(x, y) = adjacentCell (i, j)
								m = adjacentSize x
								back = adjacentCell (x, y)

		dartCells = [ (i, j) | i <- [0 .. (v - 1)], j <- [0 .. ((adjacentSize i) - 1)] ]

		faceCells = List.reverse $ fst $ foldl testCell ([], Set.empty) dartCells
			where
				testCell st@(fc, s) c
					| Set.member c s = st
					| otherwise      = (cur : fc, List.foldr Set.insert s cur)

					where
						cur = walkFace c

				walkFace start = start : (takeWhile (/= start) rest)
					where
						(_ : rest) = List.iterate (rotatedCellCW . adjacentCell) start

		f = length faceCells

		offset = Array.listArray (0, v) $ scanl (\ s i -> s + adjacentSize i) 0 [0 .. (v - 1)]

		fromCell (i, j) = Dart result $ j + (IArray.!) offset i

		dartsArr = Array.array (0, ((Array.!) offset v) - 1) $ map makeDartInfo cellWithFace
			where
				cellWithFace = List.concat $ map setPlaces $ zip faceCells [0 ..]
					where
						setPlaces (cells, faceId) = map (\ (cell, facePlace) -> (cell, (faceId, facePlace))) $ zip cells [0 ..]

				makeDartInfo ((i, j), (faceId, facePlace)) = (dartId, info)
					where
						dartId = j + (IArray.!) offset i

						info = (fromCell $ adjacentCell (i, j), (Vertex result i, j), (Face result faceId, facePlace))

		verticesArr = Array.listArray (0, v - 1) $ map (\ i -> wrapToArray $ map fromCell [ (i, j) | j <- [0 .. ((adjacentSize i) - 1)] ]) [0 .. (v - 1)]

		facesArr = Array.listArray (0, f - 1) $ map (wrapToArray . map fromCell) faceCells

		result = GraphSt dartsArr verticesArr facesArr


constructFromList :: [[(Int, Int)]] -> GraphSt
constructFromList = construct . wrapToArray2
