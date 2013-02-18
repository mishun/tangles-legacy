#include "EmbeddedTriangulation.h"

using namespace Topology;

EmbeddedTriangulation::EmbeddedTriangulation(size_t v, size_t f, size_t e)
	: EmbeddedGraph(v, f, e)
{
}

EmbeddedTriangulation::~EmbeddedTriangulation()
{
}

bool EmbeddedTriangulation::isTriangulation() const
{
	return true;
}
