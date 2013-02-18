#pragma once

#include <cstddef>
#include <cstring>
#include <Data/Native/CRC.h>

namespace Data { namespace Native {

	class BinaryBlock
	{
	private:
		const size_t _size;
		const unsigned long long _hash;
		char _data[0];

	private:
		BinaryBlock(const size_t size, const void * data)
			: _size(size)
			, _hash(Native::CRC64::evaluate(size, data))
		{
			std::memcpy(_data, data, size);
		}

		~BinaryBlock()
		{
		}

	public:
		size_t size() const
		{
			return _size;
		}

	public:
		static BinaryBlock * create(size_t, const void *);
		static void release(BinaryBlock *);
		static int compare(const BinaryBlock *, const BinaryBlock *);
	};

}}
