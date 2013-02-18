#pragma once

#include "TangleDataStructure.h"

namespace Tangles
{
	class TangleTree : public TangleDataStructure
	{
	protected:
		TangleTree(const SubTangle & sub)
			: TangleDataStructure(sub)
			, _ancestor(*this)
			, glue_position(0)
			, glue_edges(0)
		{
		}

		TangleTree(const TangleTree & father, const SubTangle & sub, D4Group orient, const size_t pos, const size_t gl)
			: TangleDataStructure(father, sub, orient, pos, gl)
			, _ancestor(father)
			, glue_position(pos)
			, glue_edges(gl)
		{
		}

		virtual ~TangleTree()
		{
		}

	public:
		const TangleTree & ancestor() const
		{
			return _ancestor;
		}

	protected:
		virtual bool preCheck(const SubTangle &, D4Group, size_t, size_t) const = 0;
		virtual bool postCheck() const = 0;

	protected:
		const TangleTree & _ancestor;
		const size_t glue_position;
		const size_t glue_edges;
	};
}
