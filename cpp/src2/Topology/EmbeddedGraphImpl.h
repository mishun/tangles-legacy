#pragma once

#include "EmbeddedGraph.h"

namespace Topology
{
	class EmbeddedGraphImpl : public EmbeddedGraph
	{
	protected:
		class VertexImpl;
		class FaceImpl;
		class EdgeImpl;

	protected:
		EmbeddedGraphImpl(size_t, size_t, size_t);
		virtual ~EmbeddedGraphImpl();

	public:
		virtual const Vertex & vertex(size_t) const;
		virtual const Face & face(size_t) const;
		virtual const Edge & edge(size_t) const;
		virtual EmbeddedGraph & dualGraph() const;

	protected:
		VertexImpl * const _vertex;
		FaceImpl * const _face;
		EdgeImpl * const _edge;

	public:
		static EmbeddedGraphImpl & createFromVertexAdjacencyList(const std::vector< std::vector< std::pair<size_t, size_t> > > &);
	};

	class EmbeddedGraphImpl::VertexImpl : public EmbeddedGraph::Vertex
	{
	public:
		struct VertexData
		{
			EdgeImpl * edge;
			size_t edge_place;
			FaceImpl * face;
			size_t face_place;
		};

	public:
		virtual size_t idInGraph() const;
		virtual size_t numberOfIncidentEdges() const;
		virtual size_t numberOfIncidentFaces() const;
		virtual const ConnectionToEdge incidentEdge(size_t) const;
		virtual const ConnectionToFace incidentFace(size_t) const;
		virtual const ConnectionToVertex adjacentVertex(size_t) const;

	public:
		void alloc(size_t sz)
		{
			size = sz;
			data = new VertexData[sz];
		}

		void release()
		{
			delete[] data;
		}

	public:
		size_t id;
		size_t size;
		VertexData * data;
	};

	class EmbeddedGraphImpl::FaceImpl : public EmbeddedGraph::Face
	{
	public:
		struct FaceData
		{
			VertexImpl * vertex;
			size_t vertex_place;
			EdgeImpl * edge;
			size_t edge_place;
		};

	public:
		virtual size_t idInGraph() const;
		virtual size_t numberOfIncidentEdges() const;
		virtual size_t numberOfIncidentVertexes() const;
		virtual const ConnectionToVertex incidentVertex(size_t) const;
		virtual const ConnectionToEdge incidentEdge(size_t) const;
		virtual const ConnectionToFace adjacentFace(size_t) const;

	public:
		void alloc(size_t sz)
		{
			size = sz;
			data = new FaceData[sz];
		}

		void release()
		{
			delete[] data;
		}

	public:
		size_t id;
		size_t size;
		FaceData * data;
	};

	class EmbeddedGraphImpl::EdgeImpl : public EmbeddedGraph::Edge
	{
	public:
		struct EdgeData
		{
			VertexImpl * vertex;
			size_t vertex_place;
			FaceImpl * face;
			size_t face_place;
		};

	public:
		virtual size_t idInGraph() const;
		virtual const ConnectionToVertex incidentVertex(size_t) const;
		virtual const ConnectionToFace incidentFace(size_t) const;

	public:
		size_t id;
		EdgeData data[2];
	};
}
