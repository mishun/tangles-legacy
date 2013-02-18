#pragma once

#include <cstddef>

namespace Numerical
{
	class IntegerData
	{
	private:
		typedef unsigned int StdType;
		typedef unsigned long long BigType;

	private:
		size_t referenceCounter;
		size_t length;
		StdType data[0];

	protected:
		void * operator new(size_t size, size_t length)
		{
			return ::operator new(size + length * sizeof(StdType));
		}

		IntegerData(size_t l)
			: referenceCounter(1)
			, length(l)
		{
		}
	};
}
