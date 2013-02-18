#include "IncrementalReducedProjection.h"

using namespace Tangles;

IncrementalReducedProjection::IncrementalReducedProjection()
	: IncrementalTangleProjection()
{
}

IncrementalReducedProjection::~IncrementalReducedProjection()
{
}

bool IncrementalReducedProjection::preCheck(const size_t legsToGlue, const LegIndex posToGlue)
{
	if(!IncrementalTangleProjection::preCheck(legsToGlue, posToGlue))
		return false;

	{
		LegIndex l = posToGlue;
		for(size_t i = 1; i < legsToGlue; i++, l = nextLegCW(l))
			if(leg(l).vertex == leg(nextLegCW(l)).vertex)
				return false;
	}

	return true;
}
