#pragma once

#include "IncrementalGeneratorBase.h"
#include "IncrementalTangleProjection.h"

namespace Tangles
{
	class IncrementalProjectionsGenerator : public IncrementalGeneratorBase
	{
	public:
		IncrementalProjectionsGenerator(size_t);
		virtual ~IncrementalProjectionsGenerator();

	protected:
		virtual void print(const IncrementalTangleProjection &);

	protected:
		virtual void startGeneration();
		virtual bool visit(const IncrementalTangleBase &);
	};
}
