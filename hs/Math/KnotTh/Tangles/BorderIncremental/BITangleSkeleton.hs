module Math.KnotTh.Tangles.BorderIncremental.BITangleSkeleton
	(
	  skeletonCreateLoner
	, skeletonGlue
	) where

import Foreign
import Foreign.C.Types

import Math.KnotTh.Tangles.TangleSkeleton


foreign import ccall "_ZN4Math6KnotTh7Tangles17BorderIncremental16BITangleSkeleton11createLonerEv"
	c_createLoner :: IO (Ptr CTangleSkeleton)

foreign import ccall "_ZNK4Math6KnotTh7Tangles17BorderIncremental16BITangleSkeleton4glueEjj"
	c_glue :: Ptr CTangleSkeleton -> CSize -> CSize -> IO (Ptr CTangleSkeleton)


skeletonCreateLoner :: TangleSkeleton
skeletonCreateLoner = wrapSkeleton c_createLoner


skeletonGlue :: TangleSkeleton -> Int -> DartSkeleton -> TangleSkeleton
skeletonGlue sk gl (DartSkeleton leg) = transformSkeleton (\ src -> c_glue src (fromIntegral gl) leg) sk
