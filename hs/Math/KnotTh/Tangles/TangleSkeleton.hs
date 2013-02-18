module Math.KnotTh.Tangles.TangleSkeleton
	(
	  CTangleSkeleton
	, TangleSkeleton
	, DartSkeleton(DartSkeleton)
	, CrossingSkeleton(CrossingSkeleton)

	, wrapSkeleton
	, transformSkeleton
	, withSkeleton
	, skeletonNumberOfCrossings
	, skeletonNumberOfLegs
	, skeletonOpposite
	, skeletonNextCCW
	, skeletonNextCW
	) where

import qualified System.IO.Unsafe as IOUnsafe
import Foreign
import Foreign.C.Types


data CTangleSkeleton


foreign import ccall "_ZNK4Math6KnotTh7Tangles14TangleSkeleton18cNumberOfCrossingsEv"
	c_numberOfCrossings :: Ptr CTangleSkeleton -> IO CSize

foreign import ccall "_ZNK4Math6KnotTh7Tangles14TangleSkeleton13cNumberOfLegsEv"
	c_numberOfLegs :: Ptr CTangleSkeleton -> IO CSize

foreign import ccall "_ZNK4Math6KnotTh7Tangles14TangleSkeleton7cNextCWENS1_4DartE"
	c_nextCW :: Ptr CTangleSkeleton -> CSize -> IO CSize

foreign import ccall "_ZNK4Math6KnotTh7Tangles14TangleSkeleton8cNextCCWENS1_4DartE"
	c_nextCCW :: Ptr CTangleSkeleton -> CSize -> IO CSize

foreign import ccall "_ZNK4Math6KnotTh7Tangles14TangleSkeleton9cOppositeENS1_4DartE"
	c_opposite :: Ptr CTangleSkeleton -> CSize -> IO CSize

foreign import ccall "&_ZN4Math6KnotTh7Tangles14TangleSkeleton7releaseEv"
	c_release :: FunPtr (Ptr CTangleSkeleton -> IO ())


newtype TangleSkeleton = TangleSkeleton (ForeignPtr CTangleSkeleton)

newtype DartSkeleton = DartSkeleton CSize

newtype CrossingSkeleton = CrossingSkeleton CSize


instance Eq DartSkeleton where
	(==) (DartSkeleton a) (DartSkeleton b) = (a == b)

instance Eq CrossingSkeleton where
	(==) (CrossingSkeleton a) (CrossingSkeleton b) = (a == b)


instance Ord CrossingSkeleton where
	compare (CrossingSkeleton a) (CrossingSkeleton b) = compare a b


wrapSkeleton :: IO (Ptr CTangleSkeleton) -> TangleSkeleton
wrapSkeleton createAction =
	TangleSkeleton $! IOUnsafe.unsafePerformIO (createAction >>= newForeignPtr c_release)


transformSkeleton :: (Ptr CTangleSkeleton -> IO (Ptr CTangleSkeleton)) -> TangleSkeleton -> TangleSkeleton
transformSkeleton transformation (TangleSkeleton sk) =
	TangleSkeleton $! IOUnsafe.unsafePerformIO (withForeignPtr sk transformation >>= newForeignPtr c_release)


{-# INLINE withSkeleton #-}
withSkeleton :: (Ptr CTangleSkeleton -> IO a) -> TangleSkeleton -> a
withSkeleton action (TangleSkeleton sk) =
	IOUnsafe.unsafePerformIO (withForeignPtr sk action)


{-# INLINE skeletonNumberOfCrossings #-}
skeletonNumberOfCrossings :: TangleSkeleton -> Int
skeletonNumberOfCrossings (TangleSkeleton sk) =
	fromIntegral $! IOUnsafe.unsafePerformIO (withForeignPtr sk c_numberOfCrossings)


{-# INLINE skeletonNumberOfLegs #-}
skeletonNumberOfLegs :: TangleSkeleton -> Int
skeletonNumberOfLegs (TangleSkeleton sk) =
	fromIntegral $! IOUnsafe.unsafePerformIO (withForeignPtr sk c_numberOfLegs)


{-# INLINE skeletonOpposite #-}
skeletonOpposite :: TangleSkeleton -> DartSkeleton -> DartSkeleton
skeletonOpposite (TangleSkeleton sk) (DartSkeleton d) =
	DartSkeleton $! IOUnsafe.unsafePerformIO (withForeignPtr sk (\ ptr -> c_opposite ptr d))


{-# INLINE skeletonNextCCW #-}
skeletonNextCCW :: TangleSkeleton -> DartSkeleton -> DartSkeleton
skeletonNextCCW (TangleSkeleton sk) (DartSkeleton d) =
	DartSkeleton $! IOUnsafe.unsafePerformIO $ withForeignPtr sk (\ ptr -> c_nextCCW ptr d)


{-# INLINE skeletonNextCW #-}
skeletonNextCW :: TangleSkeleton -> DartSkeleton -> DartSkeleton
skeletonNextCW (TangleSkeleton sk) (DartSkeleton d) =
	DartSkeleton $! IOUnsafe.unsafePerformIO $ withForeignPtr sk (\ ptr -> c_nextCW ptr d)
