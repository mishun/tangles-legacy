#include <ctime>
#include <iostream>
#include "IncrementalTemplatesGenerator.h"

using namespace Tangles;

IncrementalTemplatesGenerator::IncrementalTemplatesGenerator(size_t mc)
	: IncrementalGeneratorBase(mc)
{
}

IncrementalTemplatesGenerator::~IncrementalTemplatesGenerator()
{
}

void IncrementalTemplatesGenerator::print(const IncrementalTangleTemplate &)
{
}

void IncrementalTemplatesGenerator::startGeneration()
{
	IncrementalTangleTemplate tangle;
	incrementalBacktrack(tangle);
}

bool IncrementalTemplatesGenerator::visit(const IncrementalTangleBase & tangle)
{
	print(static_cast<const IncrementalTangleTemplate &>(tangle));
	return true;
}
