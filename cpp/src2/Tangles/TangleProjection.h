#pragma once

#include "TangleWithRootCode.h"

namespace Tangles
{
	class TangleProjection : public TangleWithRootCode
	{
	protected:
		TangleProjection(const SubTangle & sub)
			: TangleWithRootCode(sub)
			, axis_mask((sub.axis(D4Group::I) ? 1 : 0) | (sub.axis(D4Group::C) ? 2 : 0))
		{
		}

		TangleProjection(const TangleProjection & father, const SubTangle & sub, D4Group orient, const size_t pos, const size_t glue_edges)
			: TangleWithRootCode(father, sub, orient, pos, glue_edges)
			, axis_mask((legs() == 4 && father.legs() == 4) ? 1 : 0)
		{
		}

		virtual ~TangleProjection()
		{
		}

	public:
		TangleProjection * glue(const SubTangle & sub, D4Group orient, const size_t pos, const size_t glue_edges) const
		{
			if(!preCheck(sub, orient, pos, glue_edges))
				return 0;

			TangleProjection * tangle = new TangleProjection(*this, sub, orient, pos, glue_edges);
			if(tangle->postCheck())
				return tangle;
			else
			{
				delete tangle;
				return 0;
			}
		}

		const TangleProjection & ancestor() const
		{
			return static_cast<const TangleProjection &>(TangleWithRootCode::ancestor());
		}

		bool axis(D4Group g) const
		{
			assert(legs() == 4);
			size_t fset = D4Group::SubGroup::GS.factorSet(g);
			return (axis_mask & (1 << fset)) != 0;
			return false;
		}

		TangleGraph getSimpleGraph() const;
		TangleGraph getGraph() const;

	public:
		static TangleProjection * create(const SubTangle & sub)
		{
			return new TangleProjection(sub);
		}

	protected:
		virtual bool preCheck(const SubTangle &, D4Group, size_t, size_t) const;
		virtual bool postCheck() const;

	private:
		bool primeTest(const size_t) const;

	private:
		const size_t axis_mask;
	};
}
