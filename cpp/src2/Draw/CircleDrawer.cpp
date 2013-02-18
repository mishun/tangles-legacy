#include <cassert>
#include <ctime>
#include <vector>
#include <iostream>
#include <Geometry/Geometry.h>
#include <Geometry/RubberConfiguration.h>
#include <Graph/PSPrinter.h>
#include "CircleDrawer.h"

using Topology::EmbeddedGraph;
using Geometry::Vector2D;
using Geometry::Chain2D;

using namespace Draw;

std::string CircleDrawer::draw(const EmbeddedGraph & graph, size_t depth, bool relax)
{
	Graph::PSPrinter::Image ps;

	draw(ps, graph, depth, relax);

	ps.setLineWidth(0.01);
	ps.setDash(0.03, 0.03);
	ps.stroke(Graph::Circle(Vector2D(0.0, 0.0), 1.0));

	return ps.toString();
}

void CircleDrawer::draw(Graph::PSPrinter::Image & ps, const Topology::EmbeddedGraph & graph, size_t depth, bool relax)
{
	assert(graph.genus() == 0);

	std::vector<Chain2D> embedding = derivativeEmbedding(graph, depth);
	if(relax)
		embedding = relaxEmbedding(graph, embedding);

	ps.setLineWidth(0.015);
	for(size_t i = 0; i < graph.numberOfEdges(); i++)
	{
		const Chain2D & chain = embedding[i];

		Graph::Chain path;
		for(size_t j = 0; j < chain.size(); j++)
			path >> chain[j];
		ps.stroke(path);

		//for(size_t j = 1; j < chain.size(); j++)
		//	ps.circle(chain[j], 0.03);
	}
}

std::vector<Chain2D> CircleDrawer::directCalculateEmbedding(const EmbeddedGraph & graph)
{
	std::vector<Vector2D> border(graph.vertex(0).numberOfIncidentEdges());
	for(size_t i = 0; i < border.size(); i++)
		border[i] = Vector2D::polar(-2.0 * Geometry::pi * i / border.size());

	std::vector<Chain2D> embedding(graph.numberOfEdges());
	Geometry::RubberConfiguration rc(graph.numberOfVertexes() - 1);

	for(size_t i = 0; i < graph.numberOfEdges(); i++)
	{
		const EmbeddedGraph::Edge & e = graph.edge(i);
		size_t a = e.begin().vertex.idInGraph();
		size_t b = e.end().vertex.idInGraph();

		if(a == 0 && b == 0)
			continue;
		else if(a == 0)
			rc.connect(b - 1, border[e.begin().place], 1.0);
		else if(b == 0)
			rc.connect(a - 1, border[e.end().place], 1.0);
		else
			rc.connect(a - 1, b - 1, 0.5);
	}

	std::vector<Vector2D> coords = rc.solve();

	for(size_t i = 0; i < graph.numberOfEdges(); i++)
	{
		const EmbeddedGraph::Edge & e = graph.edge(i);
		embedding[i] += (e.begin().vertex.idInGraph() == 0) ? border[e.begin().place] : coords[e.begin().vertex.idInGraph() - 1];
		embedding[i] += (e.end().vertex.idInGraph() == 0) ? border[e.end().place] : coords[e.end().vertex.idInGraph() - 1];
	}

	return embedding;
}

std::vector<Chain2D> CircleDrawer::derivativeEmbedding(const EmbeddedGraph & graph, size_t depth)
{
	if(depth == 0)
		return directCalculateEmbedding(graph);

	EmbeddedGraph & derivative = graph.derivableGraph();
	std::vector<Chain2D> derivative_embedding = derivativeEmbedding(derivative, depth - 1);

	std::vector<Chain2D> embedding(graph.numberOfEdges());

	for(size_t i = 0; i < graph.numberOfEdges(); i++)
	{
		const EmbeddedGraph::Vertex & v = derivative.vertex(graph.numberOfVertexes() + graph.numberOfFaces() + i);

		Chain2D first = derivative_embedding[v.incidentEdge(0).edge.idInGraph()];
		if(v.incidentEdge(0).place == 0)
			first = first.reverse();

		Chain2D second = derivative_embedding[v.incidentEdge(2).edge.idInGraph()];
		if(v.incidentEdge(2).place == 1)
			second = second.reverse();

		embedding[i] = first + second.subChain(1, second.size() - 1);
	}

	derivative.destroy();
	return embedding;
}

namespace Draw
{
	class CircleDrawSimplifier
	{
	protected:
		struct Edge
		{
			size_t u;
			size_t v;
		};

	protected:
		const EmbeddedGraph & graph;
		size_t size;
		Vector2D * x;
		std::vector< std::vector<size_t> > indexes;
		std::vector< std::vector<size_t> > neighbours;
		std::vector<Edge> edges;

	public:
		CircleDrawSimplifier(const Topology::EmbeddedGraph & graph, const std::vector<Geometry::Chain2D> & init);
		~CircleDrawSimplifier();

	protected:
		size_t getEndpointIndex(const Chain2D &, size_t, size_t) const;
		double norm(Vector2D *) const;
		void getGradient(Vector2D *) const;
		Vector2D nextDirection(size_t, size_t) const;
		void getLimit(double *) const;

	public:
		void simplify();
		std::vector<Chain2D> result() const;
	};

	CircleDrawSimplifier::CircleDrawSimplifier(const EmbeddedGraph & _graph, const std::vector<Geometry::Chain2D> & init)
		: graph(_graph)
		, indexes(init.size())
	{
		const size_t internal_offset = graph.numberOfVertexes() - 1;

		size_t fixed_offset = internal_offset;
		for(size_t i = 0; i < init.size(); i++)
		{
			assert(init[i].size() >= 2);
			fixed_offset += init[i].size() - 2;
		}

		size = fixed_offset;
		const size_t size_with_fixed = size + graph.vertex(0).numberOfIncidentEdges();

		x = new Vector2D[size_with_fixed];

		{
			size_t offset = internal_offset;
			for(size_t i = 0; i < graph.numberOfEdges(); i++)
			{
				const Chain2D & chain = init[i];
				std::vector<size_t> & id = indexes[i];
				id.resize(chain.size());

				id[0] = getEndpointIndex(chain, i, 0);
				id[chain.size() - 1] = getEndpointIndex(chain, i, 1);

				for(size_t j = 1; j + 1 < chain.size(); j++)
				{
					x[offset] = chain[j];
					id[j] = offset;
					offset++;
				}

				for(size_t j = 0; j + 1 < id.size(); j++)
					edges.push_back((Edge){id[j], id[j + 1]});
			}
		}

		neighbours.resize(size_with_fixed);

		for(size_t i = 0; i < graph.numberOfEdges(); i++)
		{
			const std::vector<size_t> & id = indexes[i];

			for(size_t j = 1; j + 1 < id.size(); j++)
			{
				neighbours[ id[j] ].push_back(id[j + 1]);
				neighbours[ id[j] ].push_back(id[j - 1]);
			}
		}

		for(size_t i = 0; i + 1 < graph.numberOfVertexes(); i++)
		{
			const EmbeddedGraph::Vertex & v = graph.vertex(i + 1);
			for(size_t j = 0; j < v.numberOfIncidentEdges(); j++)
			{
				const size_t eid = v.incidentEdge(j).edge.idInGraph();
				const size_t place = v.incidentEdge(j).place;

				if(place == 0)
				{
					assert(indexes[eid] [0] == i);
					neighbours[i].push_back(indexes[eid] [1]);
				}
				else
					neighbours[i].push_back(indexes[eid] [indexes[eid].size() - 2]);
			}
		}

		for(size_t i = 0; i < graph.vertex(0).numberOfIncidentEdges(); i++)
		{
			const size_t eid = graph.vertex(0).incidentEdge(i).edge.idInGraph();
			const size_t place = graph.vertex(0).incidentEdge(i).place;

			if(place == 0)
				neighbours[size + i].push_back(indexes[eid] [1]);
			else
				neighbours[size + i].push_back(indexes[eid] [indexes[eid].size() - 2]);
		}
	}

	CircleDrawSimplifier::~CircleDrawSimplifier()
	{
		delete[] x;
	}

	size_t CircleDrawSimplifier::getEndpointIndex(const Chain2D & chain, size_t id, size_t endpoint) const
	{
		const EmbeddedGraph::Edge & e = graph.edge(id);
		const size_t chain_offset = (endpoint == 0) ? 0 : (chain.size() - 1);

		size_t v = e.incidentVertex(endpoint).vertex.idInGraph();
		if(v == 0)
		{
			size_t index = size + e.incidentVertex(endpoint).place;
			x[index] = chain[chain_offset];
			return index;
		}

		v--;
		x[v] = chain[chain_offset];
		return v;
	}

	double CircleDrawSimplifier::norm(Vector2D * x) const
	{
		double norm = 0.0;
		for(size_t i = 0; i < size; i++)
			norm += x[i] * x[i];
		return sqrt(norm);
	}

	void CircleDrawSimplifier::getGradient(Vector2D * grad) const
	{
		const double k_border = 10.0;
		const double k_electr = 0.24;
		const double k_bend = 10.0;
		const double k_elast = 10.0;

		double * q = new double[neighbours.size()];
		for(size_t i = 0; i < neighbours.size(); i++)
			q[i] = 0.0;

		for(size_t i = 0; i < edges.size(); i++)
		{
			const size_t a = edges[i].u;
			const size_t b = edges[i].v;

			double len = (x[a] - x[b]).abs();
			q[a] += 0.5 * len;
			q[b] += 0.5 * len;
		}

		for(size_t i = 0; i < size; i++)
			grad[i] = 0.0;

		for(size_t i = 0; i < size; i++)
		{
			double dist = x[i].abs();
			grad[i] += x[i] * (dist * log(1.0 - dist) * k_border * q[i]);
		}

		for(size_t i = 0; i < edges.size(); i++)
		{
			size_t a = edges[i].u;
			size_t b = edges[i].v;

			double s = (x[a] - x[b]).abs();
			for(size_t j = 0; j < size; j++)
			{
				if(j == a || j == b)
					continue;

				double r1 = (x[a] - x[j]).abs();
				double r2 = (x[b] - x[j]).abs();
				double d = r1 + r2;
				grad[j] += ((x[j] - x[a]) / r1 + (x[j] - x[b]) / r2) * ((s / (d * d - s * s)) * k_electr * q[j]);
			}

			if(neighbours[a].size() > 1)
			{
				grad[a] -= (x[a] * 3.0 - x[b] * 4.0 + nextDirection(a, b)) * k_bend;
				grad[a] += (x[b] - x[a]) * k_elast;
			}

			if(neighbours[b].size() > 1)
			{
				grad[b] -= (x[b] * 3.0 - x[a] * 4.0 + nextDirection(b, a)) * k_bend;
				grad[b] += (x[a] - x[b]) * k_elast;
			}
		}

		delete[] q;
	}

	Vector2D CircleDrawSimplifier::nextDirection(size_t a, size_t b) const
	{
		const std::vector<size_t> & n = neighbours[b];
		if(n.size() == 1)
			return x[b] - (x[b] - x[a]);

		if(n.size() == 2)
		{
			assert(n[0] == a || n[1] == a);
			return x[ a ^ n[0] ^ n[1] ];
		}

		size_t i = 0;
		while(n[i] != a && i < n.size())
			i++;

		assert(i < n.size());
		return x[ n[(i + n.size() / 2) % n.size()] ];
	}

	void CircleDrawSimplifier::getLimit(double * limit) const
	{
		for(size_t i = 0; i < size; i++)
			limit[i] = 1.0 - x[i].abs();

		for(size_t i = 0; i < edges.size(); i += 2)
		{
			const size_t a = edges[i].u;
			const size_t b = edges[i].v;

			double ab = (x[b] - x[a]).abs();
			Vector2D norm = (x[b] - x[a]) / ab;

			for(size_t j = 0; j < size; j++)
			{
				if(j == a || j == b)
					continue;

				double sp = norm * (x[j] - x[a]);
				double d;
				if(sp <= 0)
					d = (x[a] - x[j]).abs();
				else if(sp >= ab)
					d = (x[b] - x[j]).abs();
				else
					d = fabs(norm ^ (x[j] - x[a]));

				if(d < limit[j])
					limit[j] = d;
			}
		}
	}

	void CircleDrawSimplifier::simplify()
	{
		double time = clock();

		const double eps = 1e-2;
		const double multiple = 0.7;

		Vector2D * tmp = new Vector2D[size];
		Vector2D * tmp_grad = new Vector2D[size];
		Vector2D * grad = new Vector2D[size];
		getGradient(grad);
		const double initial_error = norm(grad);

		double speed = 1e100;
		double * limit = new double[size];

		double error = initial_error;
		bool first = true;
		size_t ok = 0, fail = 0;

		for(size_t iteration = 0; iteration < 2048; iteration++)
		{
			if(error <= eps * initial_error)
				break;

			if(first || error > 1.0)
			{
				getLimit(limit);
				for(size_t i = 0; i < size; i++)
				{
					double cur = 0.4 * limit[i] / grad[i].abs();
					if(cur < speed)
						speed = cur;
				}
			}

			for(size_t i = 0; i < size; i++)
			{
				tmp[i] = x[i];
				tmp_grad[i] = grad[i];
				x[i] += grad[i] * speed;
			}

			getGradient(grad);
			double next_error = norm(grad);

			if(next_error <= error || (!first && fail == 0))
			{
				if(next_error <= error)
				{
					first = false;
					fail = 0;
					if(++ok > 5)
					{
						speed /= multiple;
						ok = 0;
					}
				}
				else
				{
					ok = 0;
					fail++;
					speed *= multiple;
				}

				error = next_error;
			}
			else
			{
				ok = 0;
				fail++;
				speed *= multiple;

				for(size_t i = 0; i < size; i++)
				{
					x[i] = tmp[i];
					grad[i] = tmp_grad[i];
				}
			}
		}

		delete[] tmp;
		delete[] tmp_grad;
		delete[] grad;
		delete[] limit;

		time = (clock() - time) / CLOCKS_PER_SEC;
		std::cerr << "Precision = " << error / initial_error << "; time = " << time << "s\n";
	}

	std::vector<Chain2D> CircleDrawSimplifier::result() const
	{
		std::vector<Chain2D> result(graph.numberOfEdges());
		for(size_t i = 0; i < graph.numberOfEdges(); i++)
		{
			for(size_t j = 0; j < indexes[i].size(); j++)
				result[i] += x[ indexes[i] [j] ];
		}

		return result;
	}

	class TangentCircleDrawer
	{
	protected:
		const EmbeddedGraph & graph;
		double * r;

	public:
		TangentCircleDrawer(const EmbeddedGraph &);
		~TangentCircleDrawer();

	public:
		std::vector<Chain2D> calculateEmbedding();
	};

	TangentCircleDrawer::TangentCircleDrawer(const EmbeddedGraph & _graph)
		: graph(_graph)
		, r(new double[graph.numberOfVertexes()])
	{
		assert(graph.isTriangulation());
	}

	TangentCircleDrawer::~TangentCircleDrawer()
	{
		delete[] r;
	}

//	std::vector<Chain2D> TangentCircleDrawer::calculateEmbedding()
//	{
//	}
}

std::vector<Chain2D> CircleDrawer::relaxEmbedding(const Topology::EmbeddedGraph & graph, const std::vector<Geometry::Chain2D> & init)
{
	CircleDrawSimplifier sds(graph, init);
	sds.simplify();
	return sds.result();
}
