#pragma once

#include "IncrementalGeneratorBase.h"
#include "IncrementalTangleTemplate.h"

namespace Tangles
{
	class IncrementalTemplatesGenerator : public IncrementalGeneratorBase
	{
	public:
		IncrementalTemplatesGenerator(size_t);
		virtual ~IncrementalTemplatesGenerator();

	protected:
		virtual void print(const IncrementalTangleTemplate &);

	protected:
		virtual void startGeneration();
		virtual bool visit(const IncrementalTangleBase &);
	};
}
