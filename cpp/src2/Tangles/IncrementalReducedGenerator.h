#pragma once

#include "IncrementalGeneratorBase.h"
#include "IncrementalReducedProjection.h"

namespace Tangles
{
	class IncrementalReducedGenerator : public IncrementalGeneratorBase
	{
	public:
		IncrementalReducedGenerator(size_t);
		virtual ~IncrementalReducedGenerator();

	protected:
		virtual void print(const IncrementalReducedProjection &);

	protected:
		virtual void startGeneration();
		virtual bool visit(const IncrementalTangleBase &);
	};
}
