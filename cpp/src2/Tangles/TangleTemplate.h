#pragma once

#include "TangleProjection.h"

namespace Tangles
{
	class TangleTemplate : public TangleProjection
	{
	protected:
		TangleTemplate(const SubTangle & sub)
			: TangleProjection(sub)
		{
		}

		TangleTemplate(const TangleTemplate & father, const SubTangle & sub, D4Group orient, const size_t pos, const size_t glue_edges)
			: TangleProjection(father, sub, orient, pos, glue_edges)
		{
		}

		virtual ~TangleTemplate()
		{
		}

	public:
		TangleTemplate * glue(const SubTangle & sub, D4Group orient, const size_t pos, const size_t glue_edges) const
		{
			if(!preCheck(sub, orient, pos, glue_edges))
				return 0;

			TangleTemplate * tangle = new TangleTemplate(*this, sub, orient, pos, glue_edges);
			if(tangle->postCheck())
				return tangle;
			else
			{
				delete tangle;
				return 0;
			}
		}

		const TangleTemplate & ancestor() const
		{
			return static_cast<const TangleTemplate &>(TangleProjection::ancestor());
		}

	public:
		static TangleTemplate * create(const SubTangle & sub)
		{
			return new TangleTemplate(sub);
		}

	protected:
		virtual bool preCheck(const SubTangle &, D4Group, size_t, size_t) const;
		virtual bool postCheck() const;

	private:
		bool flowTest(size_t) const;
	};
}
