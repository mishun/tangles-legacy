#pragma once

#include <cstdlib>
#include <utility>
#include <string>
#include <vector>
#include <Data/IDestroyable.h>
#include <Data/UnCopyable.h>

namespace Topology
{
        //      v1 (2)          e2 (4)                                 \ v2 (4)
	//        *               |     f1 (3)                          *
	//        |               |                                    /
	// f1 (3) | f0 (1)      --*---- e1 (2)                        / e1 (3)
	//        |               |                     \            /
	//        *               |     f0 (1)           \  e0 (1)  /
	//      v0 (0)          e0 (0)             v0 (0) *--------* v1 (2)
	class EmbeddedGraph : public Data::IDestroyable, public Data::UnCopyableEquatable
	{
	public:
		class Vertex;
		class Face;
		class Edge;
		struct ConnectionToVertex;
		struct ConnectionToFace;
		struct ConnectionToEdge;

	protected:
		EmbeddedGraph(size_t, size_t, size_t);
		virtual ~EmbeddedGraph();

	public:
		size_t numberOfVertexes() const { return vertexes; }
		size_t numberOfFaces() const { return faces; }
		size_t numberOfEdges() const { return edges; }

	public:
		virtual void destroy();
		virtual const std::string toString() const;
		size_t genus() const;
		virtual bool isTriangulation() const;

		// [vertexes] .. [faces] .. [edges]
		virtual EmbeddedGraph & derivableGraph() const;

		virtual std::pair< EmbeddedGraph *, EmbeddedGraph * > sphereStarDecomposition() const;
		virtual void assertCorrectness() const;

	public:
		virtual const Vertex & vertex(size_t) const = 0;
		virtual const Face & face(size_t) const = 0;
		virtual const Edge & edge(size_t) const = 0;
		virtual EmbeddedGraph & dualGraph() const = 0;

	private:
		const size_t vertexes;
		const size_t faces;
		const size_t edges;

	public:
		static EmbeddedGraph & createFromVertexAdjacencyList(const std::vector< std::vector< std::pair<size_t, size_t> > > &);
	};

	struct EmbeddedGraph::ConnectionToVertex
	{
		ConnectionToVertex(const Vertex & v, size_t p) : vertex(v), place(p) {}

		const Vertex & vertex;
		const size_t place;
	};

	struct EmbeddedGraph::ConnectionToFace
	{
		ConnectionToFace(const Face & f, size_t p) : face(f), place(p) {}

		const Face & face;
		const size_t place;
	};

	struct EmbeddedGraph::ConnectionToEdge
	{
		ConnectionToEdge(const Edge & e, size_t p) : edge(e), place(p) {}

		const Edge & edge;
		const size_t place;
	};

	// Edge0 -> Face0 -> Edge1 -> Face1 -> ... in CCW order
	class EmbeddedGraph::Vertex : public Data::UnCopyableEquatable
	{
	public:
		virtual size_t idInGraph() const = 0;
		virtual size_t numberOfIncidentEdges() const = 0;
		virtual size_t numberOfIncidentFaces() const = 0;
		virtual const ConnectionToEdge incidentEdge(size_t) const = 0;
		virtual const ConnectionToFace incidentFace(size_t) const = 0;
		virtual const ConnectionToVertex adjacentVertex(size_t) const = 0;

	public:
		size_t nextIndexCCW(size_t index) const
		{
			return (index + 1) % numberOfIncidentEdges();
		}

		size_t nextIndexCW(size_t index) const
		{
			return (index + numberOfIncidentEdges() - 1) % numberOfIncidentEdges();
		}
	};

	// Vertex0 -> Edge0 -> Vertex1 -> Edge1 -> Vertex2 -> ... in CCW order
	class EmbeddedGraph::Face : public Data::UnCopyableEquatable
	{
	public:
		virtual size_t idInGraph() const = 0;
		virtual size_t numberOfIncidentEdges() const = 0;
		virtual size_t numberOfIncidentVertexes() const = 0;
		virtual const ConnectionToVertex incidentVertex(size_t) const = 0;
		virtual const ConnectionToEdge incidentEdge(size_t) const = 0;
		virtual const ConnectionToFace adjacentFace(size_t) const = 0;

	public:
		size_t nextIndexCCW(size_t index) const
		{
			return (index + 1) % numberOfIncidentEdges();
		}

		size_t nextIndexCW(size_t index) const
		{
			return (index + numberOfIncidentEdges() - 1) % numberOfIncidentEdges();
		}
	};

	// Vertex0 (0) -> Face0 (1) -> Vetex1 (2) -> Face1 (3) -> Vertex0 (0) in CCW order
	class EmbeddedGraph::Edge : public Data::UnCopyableEquatable
	{
	public:
		virtual size_t idInGraph() const = 0;
		virtual const ConnectionToVertex incidentVertex(size_t) const = 0;
		virtual const ConnectionToFace incidentFace(size_t) const = 0;

	public:
		const ConnectionToVertex begin() const { return incidentVertex(0); }
		const ConnectionToVertex end()   const { return incidentVertex(1); }
		const ConnectionToFace   right() const { return incidentFace(0); }
		const ConnectionToFace   left()  const { return incidentFace(1); }
	};
}
