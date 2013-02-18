#pragma once

#include <cassert>
#include <cstddef>

namespace Algebra
{
	class DnSubGroup
	{
	private:
		size_t n;
		size_t p;
		bool m;
		size_t md;

	private:
		constexpr DnSubGroup(size_t _n, size_t _p, bool _m, size_t _md)
			: n(_n)
			, p(_p)
			, m(_m)
			, md(_md)
		{
		}

	public:
		constexpr DnSubGroup()
			: n(1)
			, p(1)
			, m(false)
			, md(0)
		{
		}

		size_t order() const
		{
			return n;
		}

		size_t period() const
		{
			return p;
		}

		bool mirror() const
		{
			return m;
		}

		size_t mirrorDiff() const
		{
			return md;
		}

	public:
		static DnSubGroup createFromPeriod(size_t order, size_t period)
		{
			assert(order % period == 0);
			return DnSubGroup(order, period, false, 0);
		}

		static DnSubGroup createFromPeriodAndMirroredZero(size_t order, size_t period, size_t mirroredZero)
		{
			assert(order % period == 0);
			return DnSubGroup(order, period, true, mirroredZero % period);
		}
	};
}
