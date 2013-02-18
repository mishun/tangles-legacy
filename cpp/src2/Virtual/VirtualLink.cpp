#include <queue>
#include <vector>
#include <Util/AutoArray.h>
#include "VirtualLink.h"

using namespace Virtual;
using Tangles::IncrementalTangleBase;
using ChordDiagrams::ChordDiagram;

VirtualLink::VirtualLink(const IncrementalTangleBase & tangle, const ChordDiagram & cd, size_t rotation, bool mirror)
	: crossings(tangle.numberOfCrossings())
	, _genus(cd.genus())
{
	for(size_t i = 0; i < tangle.numberOfCrossings(); i++)
		for(size_t j = 0; j < 4; j++)
		{
			const auto n = tangle.neighbour(i + 1, j);
			if(n.vertex > 0)
			{
				v[i].neighbours[j].place = n.place;
				v[i].neighbours[j].vertex = n.vertex - 1;
			}
		}

	std::vector<IncrementalTangleBase::Dart> legs(tangle.numberOfLegs());

	{
		auto l = tangle.baseLeg();
		for(size_t i = 0; i < tangle.numberOfLegs(); i++, l = tangle.nextLegCCW(l))
			legs[i] = tangle.leg(l);
	}

	const int direction = mirror ? -1 : 1;
	for(size_t i = 0; i < tangle.numberOfLegs(); i++)
	{
		const size_t u = (2 * tangle.numberOfLegs() + rotation + direction * i) % tangle.numberOfLegs();
		const size_t v = (2 * tangle.numberOfLegs() + rotation + direction * (i + cd[i])) % tangle.numberOfLegs();

		this->v[ legs[u].vertex - 1 ].neighbours[ legs[u].place ].place = legs[v].place;
		this->v[ legs[u].vertex - 1 ].neighbours[ legs[u].place ].vertex = legs[v].vertex - 1;
	}
}

VirtualLink::~VirtualLink()
{
}

Topology::EmbeddedGraph & VirtualLink::toGraph() const
{
	std::vector< std::vector< std::pair<size_t, size_t> > > g(numberOfCrossings());

	for(size_t i = 0; i < numberOfCrossings(); i++)
	{
		g[i].resize(4);
		for(size_t j = 0; j < 4; j++)
		{
			const auto n = neighbour(i, j);

			g[i] [j].first = n.vertex;
			g[i] [j].second = n.place;
		}
	}

	return Topology::EmbeddedGraph::createFromVertexAdjacencyList(g);
}

bool VirtualLink::isPrime() const
{
	size_t n = numberOfCrossings();

	std::vector< std::vector<size_t> > g(n, std::vector<size_t>(n, 0));

	for(size_t i = 0; i < n; i++)
	{
		for(size_t j = 0; j < 4; j++)
			g[i] [ neighbour(i, j).vertex ]++;
	}

	std::vector<size_t> v(n);
	for(size_t i = 0; i < n; i++)
		v[i] = i;

	std::vector<bool> a(n);
	std::vector<size_t> w(n);
	std::vector<size_t> na(n);

	size_t best = numberOfEdges() + 1;
	while(n > 1)
	{
		a[ v[0] ] = true;
		for(size_t i = 1; i < n; i++)
		{
			a[ v[i] ] = false;
			na[i - 1] = i;
			w[i] = g[ v[0] ] [ v[i] ];
		}

		size_t prev = v[0];
		for(size_t i = 1; i < n; i++)
		{
			int zj = -1;
			for(size_t j = 1; j < n; j++)
				if(!a[v[j]] && (zj < 0 || w[j] > w[zj]))
					zj = j;

			a[ v[zj] ] = true;

			if(i + 1 == n)
			{
				if(w[zj] < best)
					best = w[zj];

				for(size_t j = 0; j < n; j++)
					g[ v[j] ] [ prev ] = g[prev] [ v[j] ] += g[ v[zj] ] [ v[j] ];
				v[zj] = v[--n];
				break;
			}

			prev = v[zj];

			for(size_t j = 1; j < n; j++)
				if(!a[ v[j] ])
					w[j] += g[ v[zj] ] [ v[j] ];
		}
	}

	return best == 4;
}

bool VirtualLink::isReducable() const
{
	for(size_t i = 0; i < numberOfCrossings(); i++)
		for(size_t j = 0; j < 4; j++)
		{
			const Dart a = neighbour(i, j);
			const Dart b = neighbour(i, (j + 1) & 3);

			if(a.vertex == b.vertex && a.place == ((b.place + 1) & 3))
				return true;
		}

	return false;
}


static void tryRootCode(const VirtualLink & link, size_t rcode[], const size_t start, const size_t e, const int dir)
{
/*	std::vector<size_t> id(link.numberOfCrossings());
	id[start] = 1;
	size_t free_id = 2;

	std::vector<size_t> queue(link.numberOfCrossings());
	size_t head = 0, tail = 0;
	queue[tail++] = (start << 2) | e;

	bool better = false;
	for( ; head < link.numberOfCrossings(); head++)
	{
		assert(head <= tail);

		size_t cur = queue[head];
		size_t ver = cur >> 2;
		size_t base = cur & 3;

		size_t vcode = 0, i = base;
		do
		{
			size_t neigh = link.neighbour(ver, i).vertex;

			if(id[neigh] == 0)
			{
				id[neigh] = free_id++;
				queue[tail++] = (neigh << 2) | link.neighbour(ver, i).place;
			}
			vcode = (vcode << 8) | id[neigh];

			i = (i + dir) & 3;
		} while(i != base);

		if(better || vcode < rcode[head])
		{
			rcode[head] = vcode;
			better = true;
		}
		else if(vcode > rcode[head])
			return;
	}*/

	std::vector<size_t> id(link.numberOfCrossings(), 0);
	std::vector<size_t> incoming(link.numberOfCrossings(), 0);

	size_t free_id = 2;
	std::queue<size_t> q;

	id[start] = 1;
	incoming[start] = e;
	q.push(start);

	bool better = false;
	for(size_t index = 0; !q.empty(); index++)
	{
		const size_t v = q.front();
		q.pop();

		size_t vcode = 0;
		for(size_t i = 0; i < 4; i++)
		{
			const size_t u = link.neighbour(v, (incoming[v] + dir * i) & 3).vertex;
			const size_t p = link.neighbour(v, (incoming[v] + dir * i) & 3).place;

			if(id[u] == 0)
			{
				id[u] = free_id++;
				incoming[u] = p;
				q.push(u);
			}

			vcode = (vcode << 8) | (id[u] << 2) | ((dir * (p - incoming[u])) & 3);
		}

		if(better || vcode < rcode[index])
		{
			rcode[index] = vcode;
			better = true;
		}
		else if(vcode > rcode[index])
			return;
	}
}

Tangles::RootCode VirtualLink::rootCode() const
{
	Util::AutoArray<size_t> rcode(numberOfCrossings(), 0xFFFFFFFF);

	for(int dir = -1; dir <= 1; dir += 2)
		for(size_t i = 0; i < numberOfCrossings(); i++)
			for(size_t j = 0; j < 4; j++)
				tryRootCode(*this, rcode.get(), i, j, dir);

	return Tangles::RootCode(numberOfCrossings(), rcode.get());
}


using Topology::EmbeddedGraph;

class Tmp
{
public:
	static bool has4LegPlanarPart(const EmbeddedGraph & g)
	{
		//for(size_t i = 0; i < g.numberOfFaces(); i++)
		//	if(bfs(g, i))
		//		return true;

		for(size_t e1 = 0; e1 < g.numberOfEdges(); e1++)
			for(size_t e2 = e1 + 1; e2 < g.numberOfEdges(); e2++)
				for(size_t e3 = e2 + 1; e3 < g.numberOfEdges(); e3++)
					for(size_t e4 = e3 + 1; e4 < g.numberOfEdges(); e4++)
					{
						if(planar(g, g.edge(e1).begin().vertex.idInGraph(), e1, e2, e3, e4) || planar(g, g.edge(e1).end().vertex.idInGraph(), e1, e2, e3, e4))
							return true;

						//for(size_t i = 0; i < g.numberOfVertexes(); i++)
						//	if(planar(g, i, e1, e2, e3, e4))
						//		return true;
					}

		return false;
	}

protected:
	static bool bfs(const EmbeddedGraph & g, size_t startFace)
	{
		std::vector<int> distance(g.numberOfFaces(), -1);
		std::vector<size_t> incomingEdge(g.numberOfFaces());
		std::vector<size_t> prevFace(g.numberOfFaces());
		std::queue<size_t> q;

		q.push(startFace);
		distance[startFace] = 0;

		while(!q.empty())
		{
			if(distance[q.front()] >= 2)
				break;

			const EmbeddedGraph::Face & f = g.face(q.front());
			q.pop();

			for(size_t i = 0; i < f.numberOfIncidentEdges(); i++)
			{
				const EmbeddedGraph::Edge & e = f.incidentEdge(i).edge;
				const EmbeddedGraph::Face & h = f.adjacentFace(i).face;

				if(distance[h.idInGraph()] >= 0)
				{
					if(distance[h.idInGraph()] == 2 && distance[f.idInGraph()] == 1 && prevFace[h.idInGraph()] != f.idInGraph())
					{
						const size_t e1 = incomingEdge[h.idInGraph()];
						const size_t e2 = incomingEdge[f.idInGraph()];
						const size_t e3 = e.idInGraph();
						const size_t e4 = incomingEdge[ prevFace[h.idInGraph()] ];

						assert(e1 != e2 && e1 != e3 && e1 != e4);
						assert(e2 != e3);
						assert(e2 != e4);
						assert(e3 != e4);

						if(planar(g, e.begin().vertex.idInGraph(), e1, e2, e3, e4) || planar(g, e.end().vertex.idInGraph(), e1, e2, e3, e4))
							return true;
					}
				}
				else
				{
					distance[h.idInGraph()] = distance[f.idInGraph()] + 1;
					incomingEdge[h.idInGraph()] = e.idInGraph();
					prevFace[h.idInGraph()] = f.idInGraph();
					q.push(h.idInGraph());
				}
			}
		}

		return false;
	}

	static bool planar(const EmbeddedGraph & g, size_t start, size_t e1, size_t e2, size_t e3, size_t e4)
	{
		std::vector<bool> vertex(g.numberOfVertexes(), false);
		std::vector<bool> edge(g.numberOfEdges(), false);
		std::vector<bool> face(g.numberOfFaces(), false);
		std::queue<size_t> q;

		face[ g.edge(e1).left().face.idInGraph() ] = true;
		face[ g.edge(e1).right().face.idInGraph() ] = true;

		face[ g.edge(e2).left().face.idInGraph() ] = true;
		face[ g.edge(e2).right().face.idInGraph() ] = true;

		face[ g.edge(e3).left().face.idInGraph() ] = true;
		face[ g.edge(e3).right().face.idInGraph() ] = true;

		face[ g.edge(e4).left().face.idInGraph() ] = true;
		face[ g.edge(e4).right().face.idInGraph() ] = true;

		q.push(start);
		vertex[start] = true;

		size_t nv = 1, ne = 4, nf = 4;

		while(!q.empty())
		{
			const EmbeddedGraph::Vertex & v = g.vertex(q.front());
			q.pop();

			for(size_t i = 0; i < v.numberOfIncidentEdges(); i++)
			{
				const EmbeddedGraph::Edge & e = v.incidentEdge(i).edge;
				const EmbeddedGraph::Vertex & u = v.adjacentVertex(i).vertex;

				bool cut = e.idInGraph() == e1 || e.idInGraph() == e2 || e.idInGraph() == e3 || e.idInGraph() == e4;

				if(cut)
					continue;

				if(!edge[e.idInGraph()])
				{
					ne++;
					edge[e.idInGraph()] = true;
				}

				{
					size_t fl = e.left().face.idInGraph();
					size_t fr = e.right().face.idInGraph();

					if(!face[fl])
					{
						nf++;
						face[fl] = true;
					}

					if(!face[fr])
					{
						nf++;
						face[fr] = true;
					}
				}

				if(!vertex[u.idInGraph()])
				{
					nv++;
					vertex[u.idInGraph()] = true;
					q.push(u.idInGraph());
				}
			}
		}

		const int euler = nv + nf - ne;

		//std::cout << euler << " " << nv << "/" << g.numberOfVertexes() << "\n";
		//if((nv > 1) && (nv < g.numberOfVertexes()))
		//	std::cout << "> " << euler << "\n";

		return (nv > 1) && (nv < g.numberOfVertexes()) && (euler == 1);
	}
};

bool VirtualLink::has4LegPlanarPart() const
{
	Topology::EmbeddedGraph & graph = toGraph();
	assert(graph.genus() == genus());
	bool result = Tmp::has4LegPlanarPart(graph);
	graph.destroy();
	return result;
}
