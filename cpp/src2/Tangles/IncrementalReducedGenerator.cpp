#include <ctime>
#include <iostream>
#include "IncrementalReducedGenerator.h"

using namespace Tangles;

IncrementalReducedGenerator::IncrementalReducedGenerator(size_t mc)
	: IncrementalGeneratorBase(mc)
{
}

IncrementalReducedGenerator::~IncrementalReducedGenerator()
{
}

void IncrementalReducedGenerator::print(const IncrementalReducedProjection &)
{
}

void IncrementalReducedGenerator::startGeneration()
{
	IncrementalReducedProjection tangle;
	incrementalBacktrack(tangle);
}

bool IncrementalReducedGenerator::visit(const IncrementalTangleBase & tangle)
{
	print(static_cast<const IncrementalReducedProjection &>(tangle));
	return true;
}
