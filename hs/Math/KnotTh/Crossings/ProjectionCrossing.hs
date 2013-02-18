module Math.KnotTh.Crossings.ProjectionCrossing
	(
	  ProjectionCrossing(..)
	) where

import qualified Math.Algebra.Group.D4 as D4

import Math.KnotTh


data ProjectionCrossing = ProjectionCrossing


instance CrossingType ProjectionCrossing where
	identifier _ = 1

	localSymmetry _ = D4.SubD4

	globalTransforms _ = D4.SubID


instance Show ProjectionCrossing where
	show _ = "+"
