#include <algorithm>
#include <memory>
#include "./BinaryBlock.h"

namespace Data { namespace Native {

	BinaryBlock * BinaryBlock::create(const size_t size, const void * src)
	{
		void * memory = ::operator new(sizeof(BinaryBlock) + size);
		return new(memory) BinaryBlock(size, src);
	}

	void BinaryBlock::release(BinaryBlock * block)
	{
		::operator delete[](block);
	}

	int BinaryBlock::compare(const BinaryBlock * a, const BinaryBlock * b)
	{
		if(a == b)
			return 0;

		if(a->_hash < b->_hash)
			return -1;

		if(a->_hash > b->_hash)
			return 1;

		if(a->_size < b->_size)
			return -1;

		if(a->_size > b->_size)
			return 1;

		return std::memcmp(a->_data, b->_data, a->_size);
	}

}}
