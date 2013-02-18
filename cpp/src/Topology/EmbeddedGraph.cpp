#include <cassert>
#include <sstream>
#include <Data/DisjointSets.h>
#include "./EmbeddedGraph.h"
#include "./SphereStarDecompCalculator.h"

using namespace Topology;

EmbeddedGraph::EmbeddedGraph(size_t v, size_t f, size_t e)
	: vertexes(v)
	, faces(f)
	, edges(e)
{
	assert(v > 0);
	assert(f > 0);
	assert(e > 0);
}

EmbeddedGraph::~EmbeddedGraph()
{
}

void EmbeddedGraph::destroy()
{
	delete this;
}

const std::string EmbeddedGraph::toString() const
{
	std::ostringstream out;
	for(size_t i = 0; i < numberOfVertexes(); i++)
	{
		const Vertex & v = vertex(i);

		out << i << ": ";
		for(size_t j = 0; j < v.numberOfIncidentEdges(); j++)
			out << "[" << v.adjacentVertex(j).vertex.idInGraph() << ", " << v.adjacentVertex(j).place << "] ";
		out << "\n";
	}

	return out.str();
}

size_t EmbeddedGraph::genus() const
{
	return (2 + numberOfEdges() - numberOfVertexes() - numberOfFaces()) / 2;
}

bool EmbeddedGraph::isTriangulation() const
{
	for(size_t i = 0; i < numberOfFaces(); i++)
	{
		const Face & f = face(i);
		if(f.numberOfIncidentEdges() != 3)
			return false;
	}
	return true;
}

EmbeddedGraph & EmbeddedGraph::derivableGraph() const
{
	std::vector< std::vector< std::pair<size_t, size_t> > > g(numberOfVertexes() + numberOfFaces() + numberOfEdges());

	for(size_t i = 0; i < numberOfVertexes(); i++)
	{
		const Vertex & v = vertex(i);
		std::vector< std::pair<size_t, size_t> > & c = g[i];
		c.resize(v.numberOfIncidentFaces() + v.numberOfIncidentEdges());

		for(size_t j = 0; j < v.numberOfIncidentEdges(); j++)
		{
			size_t neighbour = numberOfVertexes() + numberOfFaces() + v.incidentEdge(j).edge.idInGraph();
			c[2 * j] = std::make_pair(neighbour, 2 * v.incidentEdge(j).place);
		}

		for(size_t j = 0; j < v.numberOfIncidentFaces(); j++)
		{
			size_t neighbour = numberOfVertexes() + v.incidentFace(j).face.idInGraph();
			c[2 * j + 1] = std::make_pair(neighbour, 2 * v.incidentFace(j).place);
		}
	}

	for(size_t i = 0; i < numberOfFaces(); i++)
	{
		const Face & f = face(i);
		std::vector< std::pair<size_t, size_t> > & c = g[numberOfVertexes() + i];
		c.resize(f.numberOfIncidentVertexes() + f.numberOfIncidentEdges());

		for(size_t j = 0; j < f.numberOfIncidentEdges(); j++)
		{
			size_t neighbour = numberOfVertexes() + numberOfFaces() + f.incidentEdge(j).edge.idInGraph();
			c[2 * j + 1] = std::make_pair(neighbour, 2 * f.incidentEdge(j).place + 1);
		}

		for(size_t j = 0; j < f.numberOfIncidentVertexes(); j++)
		{
			size_t neighbour = f.incidentVertex(j).vertex.idInGraph();
			c[2 * j] = std::make_pair(neighbour, 2 * f.incidentVertex(j).place + 1);
		}
	}

	for(size_t i = 0; i < numberOfEdges(); i++)
	{
		const Edge & e = edge(i);
		std::vector< std::pair<size_t, size_t> > & c = g[numberOfVertexes() + numberOfFaces() + i];
		c.resize(4);

		c[0] = std::make_pair(e.begin().vertex.idInGraph(), 2 * e.begin().place);
		c[2] = std::make_pair(e.end().vertex.idInGraph(), 2 * e.end().place);
		c[1] = std::make_pair(numberOfVertexes() + e.right().face.idInGraph(), 2 * e.right().place + 1);
		c[3] = std::make_pair(numberOfVertexes() + e.left().face.idInGraph(), 2 * e.left().place + 1);
	}

	return createFromVertexAdjacencyList(g);
}

std::pair< EmbeddedGraph *, EmbeddedGraph * > EmbeddedGraph::sphereStarDecomposition() const
{
	SphereStarDecompCalculator calc(*this);
	return calc.decomposition();
}

void EmbeddedGraph::assertCorrectness() const
{
	assert(numberOfVertexes() > 0);

	for(size_t i = 0; i < numberOfVertexes(); i++)
	{
		const Vertex & v = vertex(i);
		assert(v.idInGraph() == i);
		assert(v.numberOfIncidentEdges() > 0);
		assert(v.numberOfIncidentEdges() == v.numberOfIncidentFaces());

		for(size_t j = 0; j < v.numberOfIncidentEdges(); j++)
		{
			const Edge & e = v.incidentEdge(j).edge;
			size_t back = v.incidentEdge(j).place;

			assert(back < 2);
			assert(e.incidentVertex(back).vertex == v);
			assert(e.incidentVertex(back).place == j);
		}

		for(size_t j = 0; j < v.numberOfIncidentFaces(); j++)
		{
			const Face & f = v.incidentFace(j).face;
			size_t back = v.incidentFace(j).place;
	
			assert(back < f.numberOfIncidentVertexes());
			assert(f.incidentVertex(back).vertex == v);
			assert(f.incidentVertex(back).place == j);
		}
	}

	for(size_t i = 0; i < numberOfFaces(); i++)
	{
		const Face & f = face(i);
		assert(f.idInGraph() == i);
		assert(f.numberOfIncidentEdges() > 0);
		assert(f.numberOfIncidentEdges() == f.numberOfIncidentVertexes());

		for(size_t j = 0; j < f.numberOfIncidentEdges(); j++)
		{
			const Edge & e = f.incidentEdge(j).edge;
			size_t back = f.incidentEdge(j).place;

			assert(back < 2);
			assert(e.incidentFace(back).face == f);
			assert(e.incidentFace(back).place == j);
		}

		for(size_t j = 0; j < f.numberOfIncidentVertexes(); j++)
		{
			const Vertex & v = f.incidentVertex(j).vertex;
			size_t back = f.incidentVertex(j).place;

			assert(back < v.numberOfIncidentFaces());
			assert(v.incidentFace(back).face == f);
			assert(v.incidentFace(back).place == j);
		}
	}

	for(size_t i = 0; i < numberOfEdges(); i++)
	{
		const Edge & e = edge(i);
		assert(e.idInGraph() == i);

		for(size_t j = 0; j < 2; j++)
		{
			const Vertex & v = e.incidentVertex(j).vertex;
			size_t back = e.incidentVertex(j).place;

			assert(back < v.numberOfIncidentEdges());
			assert(v.incidentEdge(back).edge == e);
			assert(v.incidentEdge(back).place == j);
		}

		for(size_t j = 0; j < 2; j++)
		{
			const Face & f = e.incidentFace(j).face;
			size_t back = e.incidentFace(j).place;

			assert(back < f.numberOfIncidentEdges());
			assert(f.incidentEdge(back).edge == e);
			assert(f.incidentEdge(back).place == j);
		}

		const Vertex & v = e.begin().vertex;
		const Vertex & u = e.end().vertex;

		assert(v.adjacentVertex(e.begin().place).vertex == u);
		assert(v.adjacentVertex(e.begin().place).place == e.end().place);
		assert(u.adjacentVertex(e.end().place).vertex == v);
		assert(u.adjacentVertex(e.end().place).place == e.begin().place);

		assert(e.left().face == v.incidentFace(e.begin().place).face);
		assert(e.left().place == v.incidentFace(e.begin().place).place);
		assert(e.right().face == u.incidentFace(e.end().place).face);
		assert(e.right().place == u.incidentFace(e.end().place).place);
	}

	{
		Data::DisjointSets comps(numberOfVertexes());
		for(size_t i = 0; i < numberOfEdges(); i++)
			comps.unionSet(edge(i).begin().vertex.idInGraph(), edge(i).end().vertex.idInGraph());

		assert(comps.numberOfSets() == 1);
	}
}
