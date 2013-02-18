module Math.Numerical.GradientDescent
	(
	  gradientDescent
	) where

import qualified Data.List as List


gradientDescent :: Int -> Double -> Double -> ([Double] -> (Double, [Double])) -> [Double] -> [Double]
gradientDescent maxIterations relativeEps absoluteEps gradient x0 = result
	where
		norm x = sqrt $ List.sum $ map (\ xi -> xi * xi) x

		(limit0, grad0) = gradient x0

		error0 = norm grad0

		iteration i x eps grad step
			| reached    = x
			| otherwise  = iteration (i + 1) next nextEps nextGrad (min nextStep nextLimit)

			where
				reached = (i >= maxIterations) || (eps < max absoluteEps (relativeEps * error0))

				next = zipWith (\ xi gi -> xi + step * gi) x grad

				(nextLimit, nextGrad) = gradient next

				nextEps = norm nextGrad

				nextStep
					| eps >= nextEps  = 1.1 * step
					| otherwise       = 0.9 * step

		result = iteration (0 :: Int) x0 error0 grad0 (0.3 * limit0)
