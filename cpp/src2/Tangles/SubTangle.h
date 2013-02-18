#pragma once

#include <cassert>
#include "D4Group.h"
#include "TangleGraph.h"

namespace Tangles
{
	class SubTangle
	{
	public:
		SubTangle(size_t cr, const D4Group::SubGroup & sym, bool v_ax, bool h_ax)
			: _crossings(cr)
			, symmetry(sym)
			, _code(free_code++)
			, axis_mask(static_cast<size_t>(v_ax) | (static_cast<size_t>(h_ax) << 1))
		{
		}

		size_t crossings() const
		{
			return _crossings;
		}

		size_t code() const
		{
			return _code;
		}

		// | 3 | 2 |
		// |  \|/  |
		// |   *   |
		// |  /|\  |
		// | 0 | 1 |
		bool axis(D4Group g) const
		{
			size_t fset = D4Group::SubGroup::GS.factorSet(g);
			return (axis_mask & (1 << fset)) != 0;
		}

		const TangleGraph getGraph() const
		{
			return *tg;
		}

	private:
		const size_t _crossings;

	public:
		const D4Group::SubGroup & symmetry;

	private:
		size_t _code;
		const size_t axis_mask;

	public:
		TangleGraph * tg;

	private:
		static size_t free_code;
	};

	class GroupSubTangle : public SubTangle
	{
	public:
		GroupSubTangle(size_t size)
			: SubTangle(size, (size == 1) ? D4Group::SubGroup::D4 : D4Group::SubGroup::GS, (size > 1), false)
		{
			assert(size > 0);
		}
	};
}
