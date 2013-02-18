module Data.BinaryBlock
	(
	  BinaryBlock
	, empty
	, fromValue
	, fromList
	) where

import qualified Data.List as List

import Foreign
import Foreign.C.Types


data CBinaryBlock


foreign import ccall "_ZN4Data11BinaryBlock6createEjPKv"
	c_create :: CSize -> Ptr a -> IO (Ptr CBinaryBlock)

foreign import ccall "&_ZN4Data11BinaryBlock7releaseEPS0_"
	c_release :: FunPtr (Ptr CBinaryBlock -> IO ())

foreign import ccall "_ZN4Data11BinaryBlock7compareEPKS0_S2_"
	c_compare :: Ptr CBinaryBlock -> Ptr CBinaryBlock -> IO CInt


newtype BinaryBlock = BinaryBlock (ForeignPtr CBinaryBlock)


instance Eq BinaryBlock where
	(==) a b = EQ == compare a b


instance Ord BinaryBlock where
	compare (BinaryBlock a) (BinaryBlock b)
		| r < 0      = LT
		| r > 0      = GT
		| otherwise  = EQ

		where
			r = unsafePerformIO $ withForeignPtr a (\ ap -> withForeignPtr b (\ bp -> c_compare ap bp))


empty :: BinaryBlock
empty = BinaryBlock $! unsafePerformIO $! do
	block <- c_create 0 nullPtr
	newForeignPtr c_release block


fromValue :: (Storable s) => s -> BinaryBlock
fromValue value = BinaryBlock $! unsafePerformIO $! do
	block <- with value (c_create size)
	newForeignPtr c_release block

	where
		size = fromIntegral $ sizeOf value


fromList :: (Storable s) => [s] -> BinaryBlock
fromList list = BinaryBlock $! unsafePerformIO $! do
	block <- withArray list (c_create size)
	newForeignPtr c_release block

	where
		size = fromIntegral $ List.sum $ map sizeOf list
