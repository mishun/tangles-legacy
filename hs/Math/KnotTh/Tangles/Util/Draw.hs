module Math.KnotTh.Tangles.Util.Draw
	(
	  drawTangle
	) where

import qualified Data.List as List
import qualified Data.Array as Array

import qualified Math.KnotTh.Crossings.ArbitraryCrossing as ArbC
import qualified Math.KnotTh.Crossings.ProjectionCrossing as ProjC

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Util.Embedding
import Graph


threadLineWidth, borderLineWidth, underCrossingSpacing :: Double
threadLineWidth = 0.04
borderLineWidth = 0.02
underCrossingSpacing = 0.12


class (CrossingType ct) => DrawableCrossingType ct where
	drawAll :: (Tangle t c d ct) => t -> (c -> (Double, Double)) -> (d -> [(Double, Double)]) -> Image ()


instance DrawableCrossingType ArbC.ArbitraryCrossing where
	drawAll tangle _ dartPath = image $ do
		mapM_ (stroke [withLineWidth threadLineWidth] . chain . edgePath) $ allEdges tangle
		return ()

		where
			edgePath (a, b) = List.concat [[rf], rest, [rl]]
				where
					path = dartPath a

					rest = let n = length path in drop 1 $ take (n - 1) path

					rf = let (p0 : p1 : _) = path in widenDart a p0 p1

					rl = let (p0 : p1 : _) = List.reverse path in widenDart b p0 p1

			widenDart d (x0, y0) (x1, y1) =
				if isLeg d || ArbC.passOver d
					then (x0, y0)
					else (x0 + dx * k / l, y0 + dy * k / l)

				where
					dx = x1 - x0
					dy = y1 - y0

					l = sqrt $ dx * dx + dy * dy

					k = min underCrossingSpacing (0.95 * l)


instance DrawableCrossingType ProjC.ProjectionCrossing where
	drawAll tangle _ dartPath = image $ do
		mapM_ (stroke [withLineWidth threadLineWidth] . chain . dartPath . fst) $ allEdges tangle
		return ()


drawTangle :: (DrawableCrossingType ct, Tangle t c d ct) => t -> Image ()
drawTangle tangle = do
	stroke [withLineWidth borderLineWidth, dashedEvenly] circumference
	drawAll tangle crossingPosition dartPath

	where
		(legEmbedding, dartEmbedding, crossingEmbedding) = tangleEmbedding tangle

		crossingPosition c = crossingEmbedding Array.! c

		dartPath d =
			if isLeg d
				then legEmbedding Array.! d
				else dartEmbedding Array.! d
