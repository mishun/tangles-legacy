#pragma once

#include <cstddef>
#include <cassert>
#include <algorithm>
#include "./nullptr.h"

namespace Data { namespace Native {

	template<typename Element>
	class AutoArray
	{
	private:
		size_t _size;
		Element * _data;

	public:
		AutoArray(size_t sz)
			: _size(sz)
			, _data(new Element[sz])
		{
		}

		AutoArray(size_t sz, const Element & value)
			: _size(sz)
			, _data(new Element[sz])
		{
			std::fill(_data, _data + _size, value);
		}

                AutoArray(AutoArray & that)
                	: _size(that._size)
                	, _data(that._data)
                {
                	that._size = 0;
                	that._data = nullptr;
                }

		~AutoArray()
		{
			delete[] _data;
		}

		AutoArray & operator=(AutoArray & that)
		{
			delete[] _data;
			_size = that._size;
			_data = that._data;
			that._size = 0;
			that._data = nullptr;
			return *this;
		}

		size_t size() const
		{
			return _size;
		}

		Element & operator[](size_t index)
		{
			assert(index < _size);
			return _data[index];
		}

		const Element & operator[](size_t index) const
		{
			assert(index < _size);
			return _data[index];
		}

		Element * get()
		{
			return _data;
		}

		const Element * get() const
		{
			return _data;
		}
	};
} }
