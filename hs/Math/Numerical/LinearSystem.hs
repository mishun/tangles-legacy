{-# LANGUAGE CPP #-}
module Math.Numerical.LinearSystem
	(
	  solveConjGrad
	, solveConjGradGeneric
	) where

import qualified Data.List as List
import qualified Data.Array as Array
import qualified System.IO.Unsafe as IOUnsafe

import Foreign
import Foreign.C.Types


#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Math9Numerical22conjugateGradientSolveEmmPKmPKdS4_Pd"
#else
foreign import ccall "_ZN4Math9Numerical22conjugateGradientSolveEjjPKjPKdS4_Pd"
#endif
	c_conjugateGradientSolve :: CSize -> CSize -> Ptr CSize -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble

--foreign import ccall "_ZN4Math9Numerical21gaussEliminationSolveEjPPdS1_S1_"
--	c_gaussEliminationSolve :: CSize -> Ptr (Ptr CDouble) -> Ptr CDouble -> Ptr CDouble -> IO CDouble


checkAndSimplify :: Int -> [((Int, Int), Double)] -> [(Int, Double)] -> ([[(Int, Double)]], [Double])
checkAndSimplify n rawA rawB
	| n <= 0     = error "solveConjGrad: n must be positive"
	| not okA    = error "solveConjGrad: matrix indices out of bound"
	| not okB    = error "solveConjGrad: vector indices out of bound"
	| otherwise  = (aList, bList)

	where
		okA = List.all (\ ((i, j), _) -> i >= 0 && i < n && j >= 0 && j < n) rawA
		okB = List.all (\ (i, _) -> i >= 0 && i < n) rawB

		aList = snd $ List.mapAccumL accum reducedA [0 .. (n - 1)]
			where
				reducedA =
					let grouped = List.groupBy (\ (i, _) (j, _) -> i == j) $ List.sort rawA
					in map (foldl1 (\ (i, a) (_, b) -> (i, a + b))) grouped 

				accum list i =
					let (cur, rest) = List.span (\ ((ci, _), _) -> ci == i) list
					in (rest, map (\ ((_, j), a) -> (j, a)) cur)

		bList = snd $ List.mapAccumL accum (List.sort rawB) [0 .. (n - 1)]
			where
				accum list i =
					let (cur, rest) = List.span (\ (ci, _) -> ci == i) list
					in (rest, List.sum $ snd $ unzip cur)


solveConjGrad :: Int -> [((Int, Int), Double)] -> [(Int, Double)] -> [Double]
solveConjGrad n rawA rawB =
	IOUnsafe.unsafePerformIO $ do
		x <- mallocArray n :: IO (Ptr CDouble)

		_ <- withArray (map realToFrac bList) $ \ b ->
			withArray (map fromIntegral nid) $ \ ids ->
				withArray (map realToFrac na) $ \ a ->
					c_conjugateGradientSolve (fromIntegral n) (fromIntegral m) ids a b x

		solution <- fmap (map realToFrac) $ peekArray n x
		free x
		return solution

	where
		(aList, bList) = checkAndSimplify n rawA rawB

		(na, nid) = unpack (List.concat $ map (\ (i, lst) -> map (\ (j, a) -> ((i, j), a)) lst) $ zip [(0 :: Int) ..] aList) ([], [])
			where
				unpack [] res = res
				unpack (((i, j), a) : rest) (ra, rid) = unpack rest (a : ra, i : j : rid)

		m = length na


solveConjGradGeneric :: Int -> [((Int, Int), Double)] -> [(Int, Double)] -> [Double]
solveConjGradGeneric n rawA rawB = conjGrad (0, initialX, initialR, initialR, delta0)
	where
		(aList, bList) = checkAndSimplify n rawA rawB

		evaluate xl = map (List.sum . map (\ (i, a) -> a * (x Array.! i))) aList
			where
				x = Array.listArray (0, n - 1) xl

		difference x = zipWith (-) bList $ evaluate x

		dot la lb = List.sum $ zipWith (*) la lb

		initialX = take n $ List.repeat 0.0
		initialR = difference initialX
		delta0 = dot initialR initialR

		updateState (k, x, d, r, delta) = (k + 1, newX, newD, newR, newDelta)
			where
				q = evaluate d
				alpha = delta / (dot d q)

				newX = zipWith (\ xi di -> xi + alpha * di) x d

				newR = if 0 == mod k 50
					then difference newX
					else zipWith (\ ri qi -> ri - alpha * qi) r q

				newDelta = dot newR newR

				newD = let beta = newDelta / delta
					in zipWith (\ ri di -> ri + beta * di) newR d

		eps = 1e-15

		conjGrad st@(k, x, _, _, delta) =
			if k > n || delta <= delta0 * eps * eps
				then x
				else conjGrad $ updateState st
