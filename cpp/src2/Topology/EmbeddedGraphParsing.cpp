#include <cassert>
#include <iostream>
#include "EmbeddedGraph.h"
#include "EmbeddedGraphImpl.h"
#include "EmbeddedTriangulation.h"

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

EmbeddedGraph & EmbeddedGraph::createRandom(Util::IRandom & random, size_t vertexes, size_t max_degree)
{
	assert(max_degree > 0);

	std::vector< std::vector< std::pair<size_t, size_t> > > graph(vertexes);

/*	for(size_t i = 0; i < vertexes; i++)
	{
		size_t degree = random.nextRange(1, max_degree);
		graph[i].resize(degree);
	}*/

	return createFromVertexAdjacencyList(graph);
}
