#pragma once

#include "IncrementalTangleProjection.h"

namespace Tangles
{
	class IncrementalReducedProjection : public IncrementalTangleProjection
	{
	public:
		IncrementalReducedProjection();
		virtual ~IncrementalReducedProjection();

	protected:
		virtual bool preCheck(size_t, LegIndex);
	};
}
