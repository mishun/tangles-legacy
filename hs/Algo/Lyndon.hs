module Algo.Lyndon
	(
	  minimumCyclicShift
	) where

import qualified Data.Array as Array


minimumCyclicShift :: (Ord a) => [a] -> (Int, [a])
minimumCyclicShift list = (shift, drop shift list ++ take shift list)
	where
		n = length list

		a = Array.listArray (0, n - 1) list

		get i = a Array.! (mod i n)

		grow i j lyn
			| i + j >= 2 * n  = (j, lyn)
			| otherwise       =
				case compare (get $ i + j) (get $ i + j - lyn) of
					GT -> grow i (j + 1) (j + 1)
					EQ -> grow i (j + 1) lyn
					LT -> (j, lyn)

		scanChunk i = if ni < n then scanChunk ni else i
			where
				(j, lyn) = grow i 1 1
				ni = i + (j - mod j lyn)

		shift = scanChunk 0
