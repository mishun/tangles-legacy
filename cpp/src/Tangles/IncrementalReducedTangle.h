#pragma once

#include "./CanonicalTangle.h"

namespace Tangles
{
	template<typename C>
	class IncrementalReducedTangle : public CanonicalTangle<C>
	{
	public:
		IncrementalReducedTangle()
		{
		}

		virtual ~IncrementalReducedTangle()
		{
		}

	protected:
		virtual bool preCheck(typename Tangle<C>::LegIterator const & posToGlue, size_t legsToGlue, const Algebra::D4Group &, const C &) //override
		{
			if(!CanonicalTangle<C>::preCheck(posToGlue, legsToGlue))
				return false;

			{
				auto l = posToGlue;
				for(size_t i = 1; i < legsToGlue; i++, l = l.nextCW())
					if(l.leg().crossing == l.nextCW().leg().crossing)
						return false;
			}

			return true;
		}
	};
}
