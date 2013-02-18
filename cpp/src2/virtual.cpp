#if 0
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <ChordDiagrams/ChordGenerator.h>
#include <Tangles/AlternatingTanglesGenerator.h>
#include <Topology/EmbeddedGraph.h>
#include <Draw/GluedSurfaceDrawer.h>
#include <Util/Util.h>
#include <Graph/psprinter.h>

using Topology::EmbeddedGraph;

Graph::PSPrinter ps("output.ps");




static size_t minCut(const Topology::EmbeddedGraph & graph)
{
	using namespace Topology;

	size_t n = graph.numberOfVertexes();

	std::vector< std::vector<size_t> > g(n, std::vector<size_t>(n, 0));

	for(size_t i = 0; i < n; i++)
	{
		const EmbeddedGraph::Vertex & v = graph.vertex(i);
		for(size_t j = 0; j < v.numberOfIncidentEdges(); j++)
			g[i] [ v.adjacentVertex(j).vertex.idInGraph() ]++;
	}

	std::vector<size_t> v(n);
	for(size_t i = 0; i < n; i++)
		v[i] = i;

	std::vector<bool> a(n);
	std::vector<size_t> w(n);
	std::vector<size_t> na(n);

	size_t best = graph.numberOfEdges() + 1;
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

	return best;
}

static bool isReduceable(const Topology::EmbeddedGraph & graph)
{
	using namespace Topology;

	for(size_t i = 0; i < graph.numberOfVertexes(); i++)
	{
		const EmbeddedGraph::Vertex & v = graph.vertex(i);
		for(size_t j = 0; j < v.numberOfIncidentEdges(); j++)
		{
			size_t k = v.nextIndexCCW(j);
			if(v.adjacentVertex(j).vertex.idInGraph() == v.adjacentVertex(k).vertex.idInGraph())
			{
				const EmbeddedGraph::Vertex & u = v.adjacentVertex(j).vertex;
				if(u.nextIndexCW(v.adjacentVertex(j).place) == v.adjacentVertex(k).place)
					return true;
			}
		}
	}

	return false;
}

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

class StarGluer : public ChordDiagrams::ChordGenerator
{
protected:
	const Tangles::AlternatingTangle & tangle;
	std::set<Tangles::RootCode> & codes;
	std::vector< std::vector<Topology::EmbeddedGraph *> > & result;
	std::vector<unsigned long long> & table;

public:
	StarGluer(const Tangles::AlternatingTangle & t, std::set<Tangles::RootCode> & c, std::vector< std::vector<Topology::EmbeddedGraph *> > & r, std::vector<unsigned long long> & tb)
		: ChordGenerator(t.legs() / 2, true)
		, tangle(t)
		, codes(c)
		, result(r)
		, table(tb)
	{
	}

protected:
	virtual bool checkChord(const ChordDiagrams::ChordDiagram & cd, size_t len, size_t u, size_t v) const
	{
		if(len == 1 || len == cd.points - 1)
			return false;

		if((len & 1) == 0)
			return false;

		return true;
	}

	virtual void print(const ChordDiagrams::ChordDiagram & cd, const Symmetry & symmetry)
	{
		const bool tryMirroredConfigurations = !symmetry.mirror && !tangle.hasMirrorSymmetry();
		const size_t period = Util::Numbers::greatestCommonDivisor(symmetry.period, tangle.legs() / tangle.getRotationSymmetry());

		for(int direction = 1; direction == 1 || (tryMirroredConfigurations && direction == -1); direction -= 2)
		{
			for(size_t rotation = 0; rotation < period; rotation++)
				testConfiguration(cd, rotation, direction);
		}
	}

	Topology::EmbeddedGraph & getTemplateGraph(const ChordDiagrams::ChordDiagram & cd, const size_t rotation, const int direction)
	{
		std::vector< std::vector< std::pair<size_t, size_t> > > g(tangle.subtangles());

		for(size_t i = 0; i < tangle.subtangles(); i++)
		{
			g[i].resize(4);
			for(size_t j = 0; j < 4; j++)
			{
				size_t neighbour = tangle.neighbour(i + 1, j);
				if(neighbour > 0)
					g[i] [j] = std::make_pair((neighbour >> 2) - 1, neighbour & 3);
			}
		}

		for(size_t i = 0; i < tangle.legs(); i++)
		{
			const size_t u = (2 * tangle.legs() + rotation + direction * i) % tangle.legs();
			const size_t v = (2 * tangle.legs() + rotation + direction * (i + cd[i])) % tangle.legs();

			g[ (tangle.border(u) >> 2) - 1 ] [ tangle.border(u) & 3 ] = std::make_pair((tangle.border(v) >> 2) - 1, tangle.border(v) & 3);
		}

		return Topology::EmbeddedGraph::createFromVertexAdjacencyList(g);
	}

	Topology::EmbeddedGraph & getGraph(const ChordDiagrams::ChordDiagram & cd, const size_t rotation, const int direction)
	{
		Tangles::TangleGraph graph = tangle.getGraph();

		std::vector< std::vector< std::pair<size_t, size_t> > > g(tangle.crossings());

		for(size_t i = 0; i < tangle.crossings(); i++)
		{
			g[i].resize(4);
			for(size_t j = 0; j < 4; j++)
			{
				size_t neighbour = graph.v[i + 1].neighbours[j];
				if(neighbour > 0)
					g[i] [j] = std::make_pair((neighbour >> 2) - 1, neighbour & 3);
			}
		}

		for(size_t i = 0; i < tangle.legs(); i++)
		{
			const size_t u = (2 * tangle.legs() + rotation + direction * i) % tangle.legs();
			const size_t v = (2 * tangle.legs() + rotation + direction * (i + cd[i])) % tangle.legs();

			g[ (graph.l[u] >> 2) - 1 ] [ graph.l[u] & 3 ] = std::make_pair((graph.l[v] >> 2) - 1, graph.l[v] & 3);
		}

		return Topology::EmbeddedGraph::createFromVertexAdjacencyList(g);
	}

	void testConfiguration(const ChordDiagrams::ChordDiagram & cd, const size_t rotation, const int direction)
	{
		Topology::EmbeddedGraph & templ = getTemplateGraph(cd, rotation, direction);

		if(processGraph(cd, templ))
		{
			const size_t genus = templ.genus();
			//Topology::EmbeddedGraph & eg = getGraph(cd, rotation, direction);
			//result[genus].push_back(&eg);

			//templ.destroy();
			result[genus].push_back(&templ);
			table[genus]++;
		}
		else
			templ.destroy();
	}

	bool processGraph(const ChordDiagrams::ChordDiagram & cd, Topology::EmbeddedGraph & templ)
	{
		if(isReduceable(templ))
			return false;

		if(minCut(templ) != 4)
			return false;

		Tangles::RootCode rcode = rootCode(templ);

		if(codes.count(rcode) != 0)
			return false;

		if(Tmp::has4LegPlanarPart(templ))
			return false;
		else
		{
			codes.insert(rcode);
			return true;
		}


		/*std::pair< Topology::EmbeddedGraph *, Topology::EmbeddedGraph * > decomp = templ.sphereStarDecomposition();
		if(decomp.second->numberOfEdges() == cd.chords)
		{
			codes.insert(rcode);
			decomp.first->destroy();
			decomp.second->destroy();
			return true;
		}
		else
		{
			decomp.first->destroy();
			decomp.second->destroy();
			return false;
		}*/
	}

	static Tangles::RootCode rootCode(const Topology::EmbeddedGraph & graph)
	{
		Util::AutoArray<size_t> rcode(graph.numberOfVertexes(), 0xFFFFFFFF);

		for(int dir = -1; dir <= 1; dir += 2)
			for(size_t i = 0; i < graph.numberOfVertexes(); i++)
			{
				assert(graph.vertex(i).numberOfIncidentEdges() == 4);

				for(size_t j = 0; j < 4; j++)
					tryRootCode(graph, rcode.get(), i, j, dir);
			}

		return Tangles::RootCode(graph.numberOfVertexes(), rcode.get());
	}

	static void tryRootCode(const Topology::EmbeddedGraph & graph, size_t rcode[], const size_t start, const size_t e, const int dir)
	{
		std::vector<size_t> id(graph.numberOfVertexes());
		id[start] = 1;
		size_t free_id = 2;

		std::vector<size_t> queue(graph.numberOfVertexes());
		size_t head = 0, tail = 0;
		queue[tail++] = (start << 2) | e;

		bool better = false;
		for( ; head < graph.numberOfVertexes(); head++)
		{
			assert(head <= tail);

			size_t cur = queue[head];
			size_t ver = cur >> 2;
			size_t base = cur & 3;

			size_t vcode = 0, i = base;
			do
			{
				size_t neigh = graph.vertex(ver).adjacentVertex(i).vertex.idInGraph();

				if(id[neigh] == 0)
				{
					id[neigh] = free_id++;
					queue[tail++] = (neigh << 2) | graph.vertex(ver).adjacentVertex(i).place;
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
		}
	}

/*	Tangles::RootCode rootCode(const Topology::EmbeddedGraph & graph)
	{
		Util::AutoArray<size_t> rcode(2 * graph.numberOfVertexes(), 0xFFFFFFFF);

		for(int dir = -1; dir <= 1; dir += 2)
			for(size_t i = 0; i < graph.numberOfVertexes(); i++)
			{
				assert(graph.vertex(i).numberOfIncidentEdges() == 4);

				for(size_t j = 0; j < 4; j++)
					tryRootCode(graph, rcode.get(), i, j, dir);
			}

		return Tangles::RootCode(2 * graph.numberOfVertexes(), rcode.get());
	}

	void tryRootCode(const Topology::EmbeddedGraph & graph, size_t rcode[], const size_t start, const size_t e, const int dir)
	{
		std::vector<size_t> id(graph.numberOfVertexes());
		id[start] = 1;
		size_t free_id = 2;

		std::vector<size_t> queue(graph.numberOfVertexes());
		size_t head = 0, tail = 0;
		queue[tail++] = (start << 2) | e;

		bool better = false;
		for( ; head < graph.numberOfVertexes(); head++)
		{
			assert(head <= tail);

			size_t cur = queue[head];
			size_t ver = cur >> 2;
			size_t base = cur & 3;

			size_t vcode = 0, i = base;
			do
			{
				size_t neigh = graph.vertex(ver).adjacentVertex(i).vertex.idInGraph();

				if(id[neigh] == 0)
				{
					id[neigh] = free_id++;
					queue[tail++] = (neigh << 2) | graph.vertex(ver).adjacentVertex(i).place;
				}
				vcode = (vcode << 4) | id[neigh];

				i = (i + dir) & 3;
			} while(i != base);

			vcode = (vcode << 3) | tangle.vertex(ver + 1).orientationCode(Tangles::D4Group(base, dir < 0));
			size_t subtangle_code = tangle.vertex(ver + 1).subtangleCode();

			if(better || subtangle_code < rcode[2 * head])
			{
				rcode[2 * head] = subtangle_code;
				better = true;
			}
			else if(subtangle_code > rcode[2 * head])
				return;

			if(better || vcode < rcode[2 * head + 1])
			{
				rcode[2 * head + 1] = vcode;
				better = true;
			}
			else if(vcode > rcode[2 * head + 1])
				return;
		}
	}*/
};

class VirtualGenerator : public Tangles::AlternatingTanglesGenerator
{
protected:
	std::set<Tangles::RootCode> codes;
	std::vector< std::vector< std::vector<Topology::EmbeddedGraph *> > > result;
	std::vector< std::vector<unsigned long long> > table;

public:
	VirtualGenerator(size_t max_v)
		: AlternatingTanglesGenerator(max_v)
		, result(max_v + 1, std::vector< std::vector<Topology::EmbeddedGraph *> >(max_v / 2 + 1))
		, table(max_v + 1, std::vector<unsigned long long>(max_v / 2 + 1))
	{
	}

	virtual void init()
	{
	}

	virtual void print(const Tangles::AlternatingTangle & tangle)
	{
		StarGluer gluer(tangle, codes, result[tangle.crossings()], table[tangle.crossings()]);
		gluer.generate();
	}

	virtual void done()
	{
		std::cout << "n\\g\t";
		for(size_t i = 1; i <= max_v / 2; i++)
			std::cout << i << "\t";
		std::cout << "total\n";

		for(size_t i = 1; i <= max_v; i++)
		{
			std::cout << i << "\t";

			unsigned long long total = 0;
			for(size_t j = 1; j <= max_v / 2; j++)
			{
				total += table[i] [j];
				std::cout << table[i] [j] << "\t";


				//if(i <= 6)
				//{
				//	for(size_t k = 0; k < result[i] [j].size(); k++)
				//		ps.print(Draw::GluedSurfaceDrawer::fastDraw(*result[i] [j] [k]));

				//	ps.newLine(false);
				//}
			}

			ps.newLine(true);

			std::cout << total << "\n";
		}
	}
};

int main()
{
	VirtualGenerator generator(6);
	generator.generate();
	return 0;
}
#endif

#include <iostream>
#include <set>
#include <Tangles/IncrementalTemplatesGenerator.h>
#include <Tangles/RootCode.h>
#include <ChordDiagrams/ChordGenerator.h>
#include <Virtual/VirtualLink.h>
#include <Util/Util.h>
#include <Graph/PSPrinter.h>
#include <Draw/GluedSurfaceDrawer.h>

Graph::PSPrinter ps("output.ps");

size_t count[32] [32];
std::set<Tangles::RootCode> codes;


bool reducable(const Topology::EmbeddedGraph & g)
{
	for(size_t i = 0; i < g.numberOfVertexes(); i++)
	{
		const Topology::EmbeddedGraph::Vertex & v = g.vertex(i);

		for(size_t j = 0; j < 2; j++)
			if(v.incidentFace(j).face.idInGraph() != v.incidentFace(j + 2).face.idInGraph())
				return true;
	}

	return false;
}

class StarGluer : public ChordDiagrams::ChordGenerator
{
protected:
	const Tangles::IncrementalTangleBase & tangle;

public:
	StarGluer(const Tangles::IncrementalTangleBase & t)
		: ChordGenerator(t.numberOfLegs() / 2, true)
		, tangle(t)
	{
	}

protected:
	virtual bool checkChord(const ChordDiagrams::ChordDiagram & cd, size_t len, size_t u, size_t v) const
	{
		if(len == 1 || len == cd.points - 1)
			return false;

		if((len & 1) == 0)
			return false;

		return true;
	}

	virtual void print(const ChordDiagrams::ChordDiagram & cd, const Symmetry & symmetry)
	{
		const bool tryMirroredConfigurations = !symmetry.mirror && !tangle.hasMirrorSymmetry();
		const size_t period = Util::Numbers::greatestCommonDivisor(symmetry.period, tangle.getPeriod());

		for(int direction = 1; direction == 1 || (tryMirroredConfigurations && direction == -1); direction -= 2)
		{
			for(size_t rotation = 0; rotation < period; rotation++)
				testConfiguration(cd, rotation, direction);
		}
	}

	void testConfiguration(const ChordDiagrams::ChordDiagram & cd, const size_t rotation, const int direction)
	{
		Virtual::VirtualLink link(tangle, cd, rotation, direction == -1);

		if(link.isReducable())
			return;

		if(!link.isPrime())
			return;

		Tangles::RootCode rcode = link.rootCode();
		if(codes.count(rcode) > 0)
			return;

		if(link.has4LegPlanarPart())
			return;

/*		{
			Topology::EmbeddedGraph & graph = link.toGraph();
			std::pair<Topology::EmbeddedGraph *, Topology::EmbeddedGraph *> decomp = graph.sphereStarDecomposition();
			size_t tmp = decomp.second->numberOfEdges();
			decomp.first->destroy();
			decomp.second->destroy();
			graph.destroy();

			if(tmp != cd.chords)
				return;
		}*/

		codes.insert(rcode);

		//if(!reducable(link.toGraph()))
		{
			count[link.numberOfCrossings()] [link.genus()]++;
			if(link.numberOfCrossings() == 4 && link.genus() == 2)
				ps.print(Draw::GluedSurfaceDrawer::fastDraw(link.toGraph()));
		}
	}
};

class VirtualGenerator : public Tangles::IncrementalTemplatesGenerator
{
public:
	VirtualGenerator(size_t mc)
		: IncrementalTemplatesGenerator(mc)
	{
		for(size_t i = 0; i < 32; i++)
			for(size_t j = 0; j < 32; j++)
				count[i] [j] = 0;
	}

protected:
	virtual void print(const Tangles::IncrementalTangleTemplate & tangle)
	{
		StarGluer gluer(tangle);
		gluer.generate();
	}

	virtual void done()
	{
		std::cerr << "VirtualGenerator::generate(): " << total() << " total";
		std::cerr << " (" << time() << "s, " << total() / time() << "t/s)\n";

		std::cout << "g\\v\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
			std::cout << i << "\t";
		std::cout << "\n";

		for(unsigned i = 1; i <= maxCrossings / 2; i++)
		{
			std::cout << i << "\t";
			for(unsigned j = 1; j <= maxCrossings; j++)
				std::cout << count[j] [i] << "\t";
			std::cout << "\n";
		}

		std::cout << "total:\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
		{
			unsigned total = 0;
			for(unsigned j = 0; j <= maxCrossings + 1; j++)
				total += count[i] [j];
			std::cout << total << "\t";
		}
		std::cout << "\n";
	}
};

int main()
{
	VirtualGenerator generator(7);
	generator.generate();
	return 0;
}
