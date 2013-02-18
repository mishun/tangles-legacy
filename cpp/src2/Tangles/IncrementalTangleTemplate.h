#pragma once

#include "IncrementalTangleProjection.h"

namespace Tangles
{
	class IncrementalTangleTemplate : public IncrementalTangleProjection
	{
	public:
		IncrementalTangleTemplate();
		virtual ~IncrementalTangleTemplate();

	protected:
		virtual bool preCheck(size_t, LegIndex);
		virtual bool postCheck(size_t);

	private:
		bool flowTest(size_t) const;
	};
}
