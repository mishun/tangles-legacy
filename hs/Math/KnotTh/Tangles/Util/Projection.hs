module Math.KnotTh.Tangles.Util.Projection
	(
	  toProjection
	) where

import qualified Math.Algebra.Group.D4 as D4
import qualified Math.KnotTh.Tangles.TangleSt as TangleSt

import Math.KnotTh.Tangles
import Math.KnotTh.Crossings.ProjectionCrossing
import Math.KnotTh.Tangles.Util.List


toProjection :: (Tangle t c d ct) => t -> TangleSt.TangleSt ProjectionCrossing
toProjection tangle = TangleSt.constructFromList (conn, map (\ _ -> (ProjectionCrossing, D4.I)) crs)
	where
		(conn, crs) = toList tangle
