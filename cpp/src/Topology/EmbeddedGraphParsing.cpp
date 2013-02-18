#include <cassert>
#include <iostream>
#include "./EmbeddedGraph.h"
#include "./EmbeddedGraphImpl.h"
#include "./EmbeddedTriangulation.h"

using namespace Topology;

EmbeddedGraph & EmbeddedGraph::createFromVertexAdjacencyList(const std::vector< std::vector< std::pair<size_t, size_t> > > & graph)
{
	EmbeddedGraph & result = EmbeddedGraphImpl::createFromVertexAdjacencyList(graph);

	if(result.isTriangulation())
	{
		return result;
	}
	else
		return result;
}
