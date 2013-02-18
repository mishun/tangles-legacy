#pragma once

#include <algorithm>

namespace Algebra
{
	template<const size_t n>
	class Vector
	{
	private:
		double v[n];

	public:
		constexpr Vector()
		{
			std::fill(v, v + n, 0.0);
		}
	};

	template<>
	class Vector<2>
	{
	public:
		double x;
		double y;
	};

	template<>
	class Vector<3>
	{
	public:
		double x;
		double y;
		double z;
	};
}
