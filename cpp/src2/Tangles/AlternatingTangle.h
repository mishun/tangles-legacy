#pragma once

#include "TangleTemplate.h"

namespace Tangles
{
	class AlternatingTangle : public TangleTemplate
	{
	protected:
		AlternatingTangle(const SubTangle & sub)
			: TangleTemplate(sub)
		{
		}

		AlternatingTangle(const AlternatingTangle & father, const SubTangle & sub, D4Group orient, const size_t pos, const size_t glue_edges)
			: TangleTemplate(father, sub, orient, pos, glue_edges)
		{
		}

		virtual ~AlternatingTangle()
		{
		}

	public:
		AlternatingTangle * glue(const SubTangle & sub, D4Group orient, const size_t pos, const size_t glue_edges) const
		{
			if(!preCheck(sub, orient, pos, glue_edges))
				return 0;

			AlternatingTangle * tangle = new AlternatingTangle(*this, sub, orient, pos, glue_edges);
			if(tangle->postCheck())
				return tangle;
			else
			{
				delete tangle;
				return 0;
			}
		}

		const AlternatingTangle & ancestor() const
		{
			return static_cast<const AlternatingTangle &>(TangleTemplate::ancestor());
		}

		bool isWeakIrreducible() const;
		std::string toString() const;

	public:
		static AlternatingTangle * create(const SubTangle & sub)
		{
			return new AlternatingTangle(sub);
		}

	protected:
		virtual bool preCheck(const SubTangle &, D4Group, size_t, size_t) const;
		virtual bool postCheck() const;
		virtual const D4Group::SubGroup & calcSymmetryGroup() const;
	};
}
