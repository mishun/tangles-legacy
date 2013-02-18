#pragma once

#include <cstddef>

namespace Util
{
	class IRandom
	{
	protected:
		virtual ~IRandom() {}

	public:
		virtual double nextDouble() = 0;
		virtual double nextNormal(double = 0.0, double = 0.0) = 0;
		virtual size_t nextRange(size_t, size_t) = 0;
	};
}
