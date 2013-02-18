{-# LANGUAGE CPP #-}
module Data.DisjointSet
	(
	  DisjointSet
	, fromBound
	, reBound
	, bound
	, findSet
	, findSet'
	, unionSet
	, unionSet'
	) where

import qualified System.IO.Unsafe as IOUnsafe
import Foreign
import Foreign.C.Types


data CDisjointSet


#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Data6Native11DisjointSet6createEm"
#else
foreign import ccall "_ZN4Data6Native11DisjointSet6createEj"
#endif
	c_create :: CSize -> IO (Ptr CDisjointSet)

#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Data6Native11DisjointSet7findSetEmPS1_"
#else
foreign import ccall "_ZN4Data6Native11DisjointSet7findSetEjPS1_"
#endif
	c_findSet :: CSize -> Ptr CDisjointSet -> IO CSize

#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Data6Native11DisjointSet8unionSetEmmPS1_"
#else
foreign import ccall "_ZN4Data6Native11DisjointSet8unionSetEjjPS1_"
#endif
	c_unionSet :: CSize -> CSize -> Ptr CDisjointSet -> IO (Ptr CDisjointSet)

foreign import ccall "&_ZN4Data6Native11DisjointSet7releaseEPS1_"
	c_release :: FunPtr (Ptr CDisjointSet -> IO ())

#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Data6Native11DisjointSet10growToSizeEmPS1_"
#else
foreign import ccall "_ZN4Data6Native11DisjointSet10growToSizeEjPS1_"
#endif
	c_growToSize :: CSize -> Ptr CDisjointSet -> IO (Ptr CDisjointSet)


data DisjointSet = DS !Int !(ForeignPtr CDisjointSet)


fromBound :: Int -> DisjointSet
fromBound n =
	DS (n + 1) $ IOUnsafe.unsafePerformIO $ do
		ptr <- c_create $ fromIntegral (n + 1)
		newForeignPtr c_release ptr


reBound :: Int -> DisjointSet -> DisjointSet
reBound newBound src@(DS n set)
	| newBound < (n - 1)   = error "reBound: can not decrease bound"
	| newBound == (n - 1)  = src
	| otherwise            =
		DS m $ IOUnsafe.unsafePerformIO $ do
			ptr <- withForeignPtr set (c_growToSize (fromIntegral m))
			newForeignPtr c_release ptr

	where
		m = newBound + 1


bound :: DisjointSet -> Int
bound (DS n _) = n - 1


findSet :: DisjointSet -> Int -> Int
findSet (DS n set) key
	| key < 0 || key >= n  = error "findSet: out of bound"
	| otherwise            = fromIntegral $ IOUnsafe.unsafePerformIO $ withForeignPtr set (c_findSet (fromIntegral key))


findSet' :: Int -> DisjointSet -> Int
findSet' key ds = findSet ds key


unionSet :: (Int, Int) -> DisjointSet -> DisjointSet
unionSet (x, y) (DS n set)
	| x < 0 || x >= n  = error "unionSet: x is out of bound"
	| y < 0 || y >= n  = error "unionSet: y is out of bound"
	| otherwise        =
		DS n $ IOUnsafe.unsafePerformIO $ do
			ptr <- withForeignPtr set (c_unionSet (fromIntegral x) (fromIntegral y))
			newForeignPtr c_release ptr


unionSet' :: DisjointSet -> (Int, Int) -> DisjointSet
unionSet' ds p = unionSet p ds
