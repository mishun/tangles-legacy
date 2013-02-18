#include "IncrementalProjectionsGenerator.h"

using namespace Tangles;

IncrementalProjectionsGenerator::IncrementalProjectionsGenerator(size_t mc)
	: IncrementalGeneratorBase(mc)
{
}

IncrementalProjectionsGenerator::~IncrementalProjectionsGenerator()
{
}

void IncrementalProjectionsGenerator::print(const IncrementalTangleProjection &)
{
}

void IncrementalProjectionsGenerator::startGeneration()
{
	IncrementalTangleProjection tangle;
	incrementalBacktrack(tangle);
}

bool IncrementalProjectionsGenerator::visit(const IncrementalTangleBase & tangle)
{
	print(static_cast<const IncrementalTangleProjection &>(tangle));
	return true;
}
