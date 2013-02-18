module Math.Algebra.RotationDirection
	(
	  RotationDirection(..)
	, both
	, isClockwise
	, isCounterClockwise
	, pickCWOrCCW
	, directionSignum
	, oppositeDirection
	) where

import Data.Ix


data RotationDirection = Clockwise | CounterClockwise deriving (Eq, Ord, Show, Enum, Ix)


both :: [RotationDirection]
both = [Clockwise, CounterClockwise]


isClockwise :: RotationDirection -> Bool
isClockwise Clockwise = True
isClockwise CounterClockwise = False


isCounterClockwise :: RotationDirection -> Bool
isCounterClockwise CounterClockwise = True
isCounterClockwise Clockwise = False


{-# INLINE pickCWOrCCW #-}
pickCWOrCCW :: (a -> b, a -> b) -> RotationDirection -> a -> b
pickCWOrCCW (cw, ccw) dir value =
	case dir of
		Clockwise        -> cw value
		CounterClockwise -> ccw value


directionSignum :: RotationDirection -> Int
directionSignum Clockwise = -1
directionSignum CounterClockwise = 1


oppositeDirection :: RotationDirection -> RotationDirection
oppositeDirection Clockwise = CounterClockwise
oppositeDirection CounterClockwise = Clockwise
