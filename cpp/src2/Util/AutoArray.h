#pragma once

#include <cstddef>

namespace Util
{
	template<class Object>
	class AutoArray
	{
	private:
		Object * const data;

	public:
		AutoArray(size_t size)
			: data(new Object[size])
		{
		}

		AutoArray(size_t size, const Object & value)
			: data(new Object[size])
		{
			for(size_t i = 0; i < size; i++)
				data[i] = value;
		}

		~AutoArray()
		{
			delete[] data;
		}

		Object * get()
		{
			return data;
		}

		const Object * get() const
		{
			return data;
		}
	};
}
