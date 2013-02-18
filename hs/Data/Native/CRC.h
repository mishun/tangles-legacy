#pragma once

#include <cstddef>

namespace Data { namespace Native {

	template<typename Type, const Type poly, const Type init, const Type final>
	class CRC
	{
	public:
		static Type evaluate(const size_t, const void *);
		static Type empty();
	};

	typedef CRC<unsigned char, 0x8CU, 0xFFU, 0xFFU> CRC8;
	typedef CRC<unsigned short, 0xA001U, 0x0000U, 0x0000U> CRC16;
	typedef CRC<unsigned long, 0xEDB88320UL, 0xFFFFFFFFUL, 0xFFFFFFFFUL> CRC32;
	typedef CRC<unsigned long long, 0xD800000000000000UL, 0xFFFFFFFFFFFFFFFFUL, 0xFFFFFFFFFFFFFFFFUL> CRC64;

}}
