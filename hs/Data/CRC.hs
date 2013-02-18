{-# LANGUAGE CPP #-}
module Data.CRC
	(
	  listCRC8
	, emptyCRC8
	, listCRC16
	, emptyCRC16
	, listCRC32
	, emptyCRC32
	, listCRC64
	, emptyCRC64
	) where

import qualified Data.List as List
import qualified System.IO.Unsafe as IOUnsafe

import Data.Word
import Foreign
import Foreign.C.Types


#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Data6Native3CRCIhLh140ELh255ELh255EE8evaluateEmPKv"
#else
foreign import ccall "_ZN4Data6Native3CRCIhLh140ELh255ELh255EE8evaluateEjPKv"
#endif
	c_crc8 :: CSize -> Ptr a -> IO CUChar

foreign import ccall "_ZN4Data6Native3CRCIhLh140ELh255ELh255EE5emptyEv"
	c_empty8 :: IO CUChar

#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Data6Native3CRCItLt40961ELt0ELt0EE8evaluateEmPKv"
#else
foreign import ccall "_ZN4Data6Native3CRCItLt40961ELt0ELt0EE8evaluateEjPKv"
#endif
	c_crc16 :: CSize -> Ptr a -> IO CUShort

foreign import ccall "_ZN4Data6Native3CRCItLt40961ELt0ELt0EE5emptyEv"
	c_empty16 :: IO CUShort

#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Data6Native3CRCImLm3988292384ELm4294967295ELm4294967295EE8evaluateEmPKv"
#else
foreign import ccall "_ZN4Data6Native3CRCImLm3988292384ELm4294967295ELm4294967295EE8evaluateEjPKv"
#endif
	c_crc32 :: CSize -> Ptr a -> IO CULong

foreign import ccall "_ZN4Data6Native3CRCImLm3988292384ELm4294967295ELm4294967295EE5emptyEv"
	c_empty32 :: IO CULong

#if x86_64_HOST_ARCH
foreign import ccall "_ZN4Data6Native3CRCIyLy15564440312192434176ELy18446744073709551615ELy18446744073709551615EE8evaluateEmPKv"
#else
foreign import ccall "_ZN4Data6Native3CRCIyLy15564440312192434176ELy18446744073709551615ELy18446744073709551615EE8evaluateEjPKv"
#endif
	c_crc64 :: CSize -> Ptr a -> IO CULLong

foreign import ccall "_ZN4Data6Native3CRCIyLy15564440312192434176ELy18446744073709551615ELy18446744073709551615EE5emptyEv"
	c_empty64 :: IO CULLong



listCRC8 :: (Storable a) => [a] -> Word8
listCRC8 list =
	let size = List.sum $ map sizeOf list
	in fromIntegral $ IOUnsafe.unsafePerformIO $ withArray list $ c_crc8 (fromIntegral size)


emptyCRC8 :: Word8
emptyCRC8 = fromIntegral $ IOUnsafe.unsafePerformIO $ c_empty8


listCRC16 :: (Storable a) => [a] -> Word16
listCRC16 list =
	let size = List.sum $ map sizeOf list
	in fromIntegral $ IOUnsafe.unsafePerformIO $ withArray list $ c_crc16 (fromIntegral size)


emptyCRC16 :: Word16
emptyCRC16 = fromIntegral $ IOUnsafe.unsafePerformIO $ c_empty16


listCRC32 :: (Storable a) => [a] -> Word32
listCRC32 list =
	let size = List.sum $ map sizeOf list
	in fromIntegral $ IOUnsafe.unsafePerformIO $ withArray list $ c_crc32 (fromIntegral size)


emptyCRC32 :: Word32
emptyCRC32 = fromIntegral $ IOUnsafe.unsafePerformIO $ c_empty32


listCRC64 :: (Storable a) => [a] -> Word64
listCRC64 list =
	let size = List.sum $ map sizeOf list
	in fromIntegral $ IOUnsafe.unsafePerformIO $ withArray list $ c_crc64 (fromIntegral size)


emptyCRC64 :: Word64
emptyCRC64 = fromIntegral $ IOUnsafe.unsafePerformIO $ c_empty64
