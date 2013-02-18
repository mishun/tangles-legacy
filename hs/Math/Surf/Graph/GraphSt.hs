module Math.Surf.Graph.GraphSt
	(
	  GraphSt(..)
	, Vertex
	, Face
	, Dart
	) where

import qualified Data.List as List
import qualified Data.Array as Array

import Math.Surf.Graph
import Math.Surf.Graph.Dart
import Math.Surf.Graph.Vertex
import Math.Surf.Graph.Face


data GraphSt = GraphSt
	{
		_darts :: Array.Array Int ((Dart GraphSt), (Vertex GraphSt, Int), (Face GraphSt, Int)),
		_vertices :: Array.Array Int (Array.Array Int (Dart GraphSt)),
		_faces :: Array.Array Int (Array.Array Int (Dart GraphSt))
	}


_dartInfo :: Dart GraphSt -> (Dart GraphSt, (Vertex GraphSt, Int), (Face GraphSt, Int))
_dartInfo d = (_darts $ _dartOwner d) Array.! (_dartIndex d)

_vertexInfo :: Vertex GraphSt -> Array.Array Int (Dart GraphSt)
_vertexInfo v = (_vertices $ _vertexOwner v) Array.! (_vertexIndex v)

_faceInfo :: Face GraphSt -> Array.Array Int (Dart GraphSt)
_faceInfo f = (_faces $ _faceOwner f) Array.! (_faceIndex f)


instance SurfaceGraph GraphSt (Vertex GraphSt) (Face GraphSt) (Dart GraphSt) where

	rotateCCW d = vertexDart (mod (place + 1) nv) ver
		where
			(ver, place) = begin d
			nv = numberOfVertexDarts ver

	rotateCW d = vertexDart (mod (place + nv - 1) nv) ver
		where
			(ver, place) = begin d
			nv = numberOfVertexDarts ver

	walkCCW d = faceDart (mod (place + 1) nf) fc
		where
			(fc, place) = left d
			nf = numberOfFaceDarts fc

	walkCW d = faceDart (mod (place + nf - 1) nf) fc
		where
			(fc, place) = left d
			nf = numberOfFaceDarts fc

	opposite d = let (opp, _, _) = _dartInfo d in opp

	begin d = let (_, beg, _) = _dartInfo d in beg

	left d = let (_, _, lft) = _dartInfo d in lft

	numberOfVertices g = 1 + (snd $ Array.bounds $ _vertices g)

	numberOfFaces g = 1 + (snd $ Array.bounds $ _faces g)

	numberOfEdges g = div (1 + (snd $ Array.bounds $ _darts g)) 2

	verticesRange g =
		let v = numberOfVertices g
		in (Vertex g 0, Vertex g $ v - 1)

	facesRange g =
		let f = numberOfFaces g
		in (Face g 0, Face g $ f - 1)

	dartsRange g =
		let e = numberOfEdges g
		in (Dart g 0, Dart g $ 2 * e - 1)

	numberOfVertexDarts v = 1 + (snd $ Array.bounds $ _vertexInfo v)

	numberOfFaceDarts f = 1 + (snd $ Array.bounds $ _faceInfo f)

	vertexIndex = _vertexIndex

	faceIndex = _faceIndex

	dartIndex = _dartIndex

	vertexDart ind v =
		if ind >= 0 && ind < vn
			then ds Array.! ind
			else error "vertexDart: out of bound"

		where
			ds = _vertexInfo v
			vn = 1 + (snd $ Array.bounds ds)

	faceDart ind f =
		if ind >= 0 && ind < fn
			then ds Array.! ind
			else error "faceDart: out of bound"

		where
			ds = _faceInfo f
			fn = 1 + (snd $ Array.bounds ds)

	vertex ind g =
		if ind >= 0 && ind < v
			then Vertex g ind
			else error "vertes: out of bound"

		where
			v = numberOfVertices g

	face ind g =
		if ind >= 0 && ind < f
			then Face g ind
			else error "faces: out of bound"

		where
			f = numberOfFaces g


instance Show GraphSt where
	show = List.intercalate "\n" . map showVertex . allVertices
		where
			showVertex v = List.concat [show $ vertexIndex v, " -> {", inc, "}"]
				where
					inc = List.intercalate ", " $ map (show . toPair . opposite) $ vertexIncidentDarts v
