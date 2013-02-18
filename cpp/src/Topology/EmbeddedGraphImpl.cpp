#include <cassert>
#include "./EmbeddedGraphImpl.h"

using namespace Topology;

EmbeddedGraphImpl::EmbeddedGraphImpl(size_t v, size_t f, size_t e)
	: EmbeddedGraph(v, f, e)
	, _vertex(new VertexImpl[ numberOfVertexes() ])
	, _face(new FaceImpl[ numberOfFaces() ])
	, _edge(new EdgeImpl[ numberOfEdges() ])
{
	for(size_t i = 0; i < numberOfVertexes(); i++)
	{
		_vertex[i].id = i;
	}

	for(size_t i = 0; i < numberOfFaces(); i++)
	{
		_face[i].id = i;
	}

	for(size_t i = 0; i < numberOfEdges(); i++)
	{
		_edge[i].id = i;
	}
}

EmbeddedGraphImpl::~EmbeddedGraphImpl()
{
	for(size_t i = 0; i < numberOfVertexes(); i++)
		_vertex[i].release();

	for(size_t i = 0; i < numberOfFaces(); i++)
		_face[i].release();

	delete[] _vertex;
	delete[] _face;
	delete[] _edge;
}

const EmbeddedGraph::Vertex & EmbeddedGraphImpl::vertex(size_t index) const
{
	assert(index < numberOfVertexes());
	return _vertex[index];
}

const EmbeddedGraph::Face & EmbeddedGraphImpl::face(size_t index) const
{
	assert(index < numberOfFaces());
	return _face[index];
}

const EmbeddedGraph::Edge & EmbeddedGraphImpl::edge(size_t index) const
{
	assert(index < numberOfEdges());
	return _edge[index];
}

EmbeddedGraph & EmbeddedGraphImpl::dualGraph() const
{
	EmbeddedGraphImpl & dual = *new EmbeddedGraphImpl(numberOfFaces(), numberOfVertexes(), numberOfEdges());

	for(size_t i = 0; i < numberOfVertexes(); i++)
	{
		const Vertex & v = vertex(i);
		FaceImpl & df = dual._face[i];
		df.alloc(v.numberOfIncidentEdges());

		for(size_t j = 0; j < v.numberOfIncidentEdges(); j++)
		{
			df.data[j].edge = &dual._edge[ v.incidentEdge(j).edge.idInGraph() ];
			df.data[j].edge_place = v.incidentEdge(j).place ^ 1;
		}

		for(size_t j = 0; j < v.numberOfIncidentFaces(); j++)
		{
			size_t id = (j + v.numberOfIncidentFaces() - 1) % v.numberOfIncidentFaces();
			df.data[j].vertex = &dual._vertex[ v.incidentFace(id).face.idInGraph() ];

			size_t sz = v.incidentFace(id).face.numberOfIncidentVertexes();
			df.data[j].vertex_place = (v.incidentFace(id).place + sz - 1) % sz;
		}
	}

	for(size_t i = 0; i < numberOfFaces(); i++)
	{
		const Face & f = face(i);
		VertexImpl & dv = dual._vertex[i];
		dv.alloc(f.numberOfIncidentEdges());

		for(size_t j = 0; j < f.numberOfIncidentEdges(); j++)
		{
			dv.data[j].edge = &dual._edge[ f.incidentEdge(j).edge.idInGraph() ];
			dv.data[j].edge_place = f.incidentEdge(j).place;
		}

		for(size_t j = 0; j < f.numberOfIncidentVertexes(); j++)
		{
			size_t id = (j + 1) % f.numberOfIncidentVertexes();
			dv.data[j].face = &dual._face[ f.incidentVertex(id).vertex.idInGraph() ];

			size_t sz = f.incidentVertex(id).vertex.numberOfIncidentFaces();
			dv.data[j].face_place = (f.incidentVertex(id).place + 1) % sz;
		}
	}

	for(size_t i = 0; i < numberOfEdges(); i++)
	{
		const Edge & e = edge(i);
		EdgeImpl & de = dual._edge[i];

		de.data[0].vertex = &dual._vertex[ e.right().face.idInGraph() ];
		de.data[0].vertex_place = e.right().place;
		de.data[0].face = &dual._face[ e.end().vertex.idInGraph() ];
		de.data[0].face_place = e.end().place;

		de.data[1].vertex = &dual._vertex[ e.left().face.idInGraph() ];
		de.data[1].vertex_place = e.left().place;
		de.data[1].face = &dual._face[ e.begin().vertex.idInGraph() ];
		de.data[1].face_place = e.begin().place;
	}

	dual.assertCorrectness();
	return dual;
}

EmbeddedGraphImpl & EmbeddedGraphImpl::createFromVertexAdjacencyList(const std::vector< std::vector< std::pair<size_t, size_t> > > & graph)
{
	std::vector< std::vector<size_t> > id(graph.size());

	const size_t vertexes = graph.size();

	size_t free_id = 0;
	for(size_t i = 0; i < graph.size(); i++)
	{
		assert(graph[i].size() > 0);
		id[i].resize(graph[i].size());
		for(size_t j = 0; j < graph[i].size(); j++)
		{
			id[i] [j] = free_id++;

			size_t neighbour = graph[i] [j].first;
			size_t position = graph[i] [j].second;

			assert(neighbour < graph.size());
			assert(position < graph[neighbour].size());
			assert(graph[neighbour] [position] == std::make_pair(i, j));
		}
	}

	assert(free_id % 2 == 0);
	std::vector<size_t> edge_id(free_id, 0);
	std::vector<size_t> face_id(free_id, 0);

	size_t faces = 0;
	size_t edges = 0;

	for(size_t i = 0; i < graph.size(); i++)
		for(size_t j = 0; j < graph[i].size(); j++)
		{
			if(edge_id[ id[i] [j] ] == 0)
			{
				edges++;
				edge_id[ id[i] [j] ] = edges;
				size_t y = graph[i] [j].first;
				size_t x = graph[i] [j].second;
				edge_id[ id[y] [x] ] = edges;
			}

			if(face_id[ id[i] [j] ] == 0)
			{
				faces++;
				for(size_t y = i, x = j; face_id[ id[y] [x] ] == 0; )
				{
					face_id[ id[y] [x] ] = faces;
					std::pair<size_t, size_t> next = graph[y] [x];
					y = next.first;
					x = (next.second + graph[y].size() - 1) % graph[y].size();
				}
			}
		}

	std::vector<size_t> face_size(faces, 0);
	for(size_t i = 0; i < face_id.size(); i++)
		face_size[ face_id[i] - 1 ]++;

	EmbeddedGraphImpl & eg = *new EmbeddedGraphImpl(vertexes, faces, edges);

	for(size_t i = 0; i < eg.numberOfVertexes(); i++)
		eg._vertex[i].alloc(graph[i].size());

	for(size_t i = 0; i < eg.numberOfFaces(); i++)
		eg._face[i].alloc(face_size[i]);


	std::vector<bool> edge_processed(free_id, false);
	std::vector<bool> face_processed(free_id, false);

	for(size_t y0 = 0; y0 < graph.size(); y0++)
		for(size_t x0 = 0; x0 < graph[y0].size(); x0++)
		{
			if(face_processed[ id[y0] [x0] ])
				continue;

			size_t fid = face_id[ id[y0] [x0] ] - 1;
			FaceImpl & f = eg._face[fid];

			for(size_t y = y0, x = x0, num = 0; !face_processed[ id[y] [x] ]; num++)
			{
				size_t eid = edge_id[ id[y] [x] ] - 1;
				size_t ny = graph[y] [x].first;
				size_t nx = graph[y] [x].second;

				VertexImpl & v = eg._vertex[y];
				VertexImpl & u = eg._vertex[ny];
				EdgeImpl & e = eg._edge[eid];

				if(!edge_processed[ id[y] [x] ])
				{
					e.data[0].vertex = &v;
					e.data[1].vertex = &u;
					e.data[0].vertex_place = x;
					e.data[1].vertex_place = nx;

					v.data[x].edge = &e;
					u.data[nx].edge = &e;
					v.data[x].edge_place = 0;
					u.data[nx].edge_place = 1;

					e.data[1].face = &f;
					e.data[1].face_place = num;
					f.data[num].edge = &e;
					f.data[num].edge_place = 1;

					edge_processed[ id[y] [x] ] = true;
					edge_processed[ id[ny] [nx] ] = true;
				}
				else
				{
					e.data[0].face = &f;
					e.data[0].face_place = num;
					f.data[num].edge = &e;
					f.data[num].edge_place = 0;
				}

				f.data[num].vertex = &v;
				f.data[num].vertex_place = x;
				v.data[x].face = &f;
				v.data[x].face_place = num;

				face_processed[ id[y] [x] ] = true;
				y = ny;
				x = (nx + graph[ny].size() - 1) % graph[ny].size();
			}
		}

	eg.assertCorrectness();
	return eg;
}


size_t EmbeddedGraphImpl::VertexImpl::idInGraph() const
{
	return id;
}

size_t EmbeddedGraphImpl::VertexImpl::numberOfIncidentEdges() const
{
	return size;
}

size_t EmbeddedGraphImpl::VertexImpl::numberOfIncidentFaces() const
{
	return size;
}

const EmbeddedGraph::ConnectionToEdge EmbeddedGraphImpl::VertexImpl::incidentEdge(size_t index) const
{
	assert(index < numberOfIncidentEdges());
	return ConnectionToEdge(*data[index].edge, data[index].edge_place);
}

const EmbeddedGraph::ConnectionToFace EmbeddedGraphImpl::VertexImpl::incidentFace(size_t index) const
{
	assert(index < numberOfIncidentFaces());
	return ConnectionToFace(*data[index].face, data[index].face_place);
}

const EmbeddedGraph::ConnectionToVertex EmbeddedGraphImpl::VertexImpl::adjacentVertex(size_t index) const
{
	assert(index < numberOfIncidentEdges());
	ConnectionToEdge cte = incidentEdge(index);
	return cte.edge.incidentVertex(cte.place ^ 1);
}


size_t EmbeddedGraphImpl::FaceImpl::idInGraph() const
{
	return id;
}

size_t EmbeddedGraphImpl::FaceImpl::numberOfIncidentEdges() const
{
	return size;
}

size_t EmbeddedGraphImpl::FaceImpl::numberOfIncidentVertexes() const
{
	return size;
}

const EmbeddedGraph::ConnectionToVertex EmbeddedGraphImpl::FaceImpl::incidentVertex(size_t index) const
{
	assert(index < numberOfIncidentVertexes());
	return ConnectionToVertex(*data[index].vertex, data[index].vertex_place);
}

const EmbeddedGraph::ConnectionToEdge EmbeddedGraphImpl::FaceImpl::incidentEdge(size_t index) const
{
	assert(index < numberOfIncidentEdges());
	return ConnectionToEdge(*data[index].edge, data[index].edge_place);
}

const EmbeddedGraph::ConnectionToFace EmbeddedGraphImpl::FaceImpl::adjacentFace(size_t index) const
{
	assert(index < numberOfIncidentEdges());
	ConnectionToEdge cte = incidentEdge(index);
	return cte.edge.incidentFace(cte.place ^ 1);
}


size_t EmbeddedGraphImpl::EdgeImpl::idInGraph() const
{
	return id;
}

const EmbeddedGraph::ConnectionToVertex EmbeddedGraphImpl::EdgeImpl::incidentVertex(size_t index) const
{
	assert(index < 2);
	return ConnectionToVertex(*data[index].vertex, data[index].vertex_place);
}

const EmbeddedGraph::ConnectionToFace EmbeddedGraphImpl::EdgeImpl::incidentFace(size_t index) const
{
	assert(index < 2);
	return ConnectionToFace(*data[index].face, data[index].face_place);
}
