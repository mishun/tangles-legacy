#include <cassert>
#include <queue>
#include "TangleGraph.h"

using namespace Tangles;

TangleGraph::TangleGraph()
	: v(2)
	, l(4)
{
	v[1].neighbours[0] = 0;
	v[1].neighbours[1] = 0;
	v[1].neighbours[2] = 0;
	v[1].neighbours[3] = 0;

	l[0] = 4 * 1 + 0;
	l[1] = 4 * 1 + 1;
	l[2] = 4 * 1 + 2;
	l[3] = 4 * 1 + 3;

	parts = 1;
}

TangleGraph::TangleGraph(size_t vs, size_t ls)
	: v(vs + 1)
	, l(ls)
{
	parts = 1;
}

TangleGraph TangleGraph::transform(size_t rotation, bool mirror) const
{
	rotation = rotation % legs();

	TangleGraph res(vertexes(), legs());

	for(size_t i = 1; i <= vertexes(); i++)
		res.v[i] = v[i];

	for(size_t i = 0; i < legs(); i++)
		res.l[i] = l[ (rotation + i) % legs() ];

	if(mirror)
	{
		for(size_t i = 1; i <= vertexes(); i++)
		{
			std::reverse(res.v[i].neighbours, res.v[i].neighbours + 4);
			for(size_t j = 0; j < 4; j++)
				res.v[i].neighbours[j] = (res.v[i].neighbours[j] & ~3) | (3 - (res.v[i].neighbours[j] & 3));
		}

		for(size_t i = 0; i < legs(); i++)
			res.l[i] = (res.l[i] & ~3) | (3 - (res.l[i] & 3));

		for(size_t i = 1, j = legs() - 1; i < j; i++, j--)
			std::swap(res.l[i], res.l[j]);
	}

	return res;
}

TangleGraph TangleGraph::substitute(const std::vector<TangleGraph> & subtangles) const
{
	assert(subtangles.size() == vertexes() + 1);

	size_t total = 0;
	for(size_t i = 1; i < subtangles.size(); i++)
	{
		assert(subtangles[i].legs() == 4);
		total += subtangles[i].vertexes();
	}

	std::vector<size_t> offset(subtangles.size(), 0);
	for(size_t i = 2; i < subtangles.size(); i++)
		offset[i] = offset[i - 1] + subtangles[i - 1].vertexes();

	TangleGraph res(total, legs());

	for(size_t k = 1; k <= vertexes(); k++)
	{
		const TangleGraph & g = subtangles[k];
		size_t off = offset[k];

		for(size_t i = 1; i <= g.vertexes(); i++)
			for(size_t j = 0; j < 4; j++)
				if(g.v[i].neighbours[j] != 0)
					res.v[off + i].neighbours[j] = g.v[i].neighbours[j] + off * 4;
	}

	for(size_t i = 1; i <= vertexes(); i++)
		for(size_t j = 0; j < 4; j++)
		{
			size_t k = v[i].neighbours[j] >> 2;
			size_t l = v[i].neighbours[j] & 3;

			res.v[ offset[i] + (subtangles[i].l[j] >> 2) ].neighbours[ subtangles[i].l[j] & 3 ] = subtangles[k].l[l] + offset[k] * 4;
		}

	for(size_t i = 0; i < legs(); i++)
	{
		size_t tmp = l[i];
		res.l[i] = subtangles[tmp >> 2].l[tmp & 3] + offset[tmp >> 2] * 4;
		res.v[ res.l[i] >> 2 ].neighbours[ res.l[i] & 3 ] = 0;
	}

	res.parts = subtangles.size() - 1;
	return res;
}

Topology::EmbeddedGraph & TangleGraph::toEmbeddedGraph() const
{
	std::vector< std::vector< std::pair<size_t, size_t> > > g(vertexes() + 1);

	for(size_t i = 1; i <= vertexes(); i++)
	{
		g[i].resize(4);
		for(size_t j = 0; j < 4; j++)
		{
			size_t n = v[i].neighbours[j];
			if(n != 0)
				g[i] [j] = std::make_pair(n >> 2, n & 3);
		}
	}

	g[0].resize(legs());
	for(size_t i = 0; i < legs(); i++)
	{
		size_t n = l[legs() - 1 - i];
		g[0] [i] = std::make_pair(n >> 2, n & 3);
		g[n >> 2] [n & 3] = std::make_pair(0, i);
	}

	return Topology::EmbeddedGraph::createFromVertexAdjacencyList(g);
}

RootCode TangleGraph::getCode() const
{
	std::vector<size_t> rcode(vertexes(), 0xFFFFFFFF);
	for(size_t j = 1; j <= vertexes(); j++)
		for(size_t i = 0; i < 4; i++)
		{
			tryRootCode(&rcode[0], j, i, 1);
			tryRootCode(&rcode[0], j, i, -1);
		}

	return RootCode(vertexes(), &rcode[0]);
}

int TangleGraph::tryRootCode(size_t rcode[], const size_t start, const size_t e, const int dir) const
{
	assert(1 <= start && start <= vertexes());
	assert(e < 4);
	assert(dir == 1 || dir == -1);

	std::vector<size_t> id(vertexes() + 1, 0);
	id[start] = 1;

	std::queue<size_t> q;
	q.push((start << 2) | e);

	size_t free_id = 2;
	bool better = false;
	for(size_t j = 0; !q.empty(); j++)
	{
		size_t cur = q.front();
		q.pop();

		size_t ver = cur >> 2;
		size_t base = cur & 3;

		size_t vcode = 0, i = base;
		do
		{
			size_t neigh = v[ver].neighbours[i] >> 2;
			vcode <<= 8;
			if(neigh != 0)
			{
				if(id[neigh] == 0)
				{
					id[neigh] = free_id++;
					q.push(v[ver].neighbours[i]);
				}
				vcode |= id[neigh];
			}
			i = (i + dir) & 3;
		} while(i != base);

		if(better || vcode < rcode[j])
		{
			rcode[j] = vcode;
			better = true;
		}
		else if(vcode > rcode[j])
			return -1;
	}

	if(better)
		return 1;
	return 0;
}

bool TangleGraph::chordDiagram() const
{
	assert(l.size() == 4);

	size_t i = l[0], p = 0;
	while(i != 0)
	{
		p = (i & ~3) | ((i + 2) & 3);
		i = v[p >> 2].neighbours[p & 3];
	}
	return l[2] == p;
}

size_t TangleGraph::internalComponents() const
{
	std::vector<bool> visited(v.size() * 4, false);

	for(size_t i = 0; i < l.size(); i++)
	{
		size_t c = l[i];
		while(c != 0 && !visited[c])
		{
			visited[c] = visited[c ^ 2] = true;
			c = v[c >> 2].neighbours[ (c ^ 2) & 3 ];
		}
	}

	size_t res = 0;
	for(size_t i = 4; i < visited.size(); i++)
		if(!visited[i])
		{
			size_t c = i;
			while(c != 0 && !visited[c])
			{
				visited[c] = visited[c ^ 2] = true;
				c = v[c >> 2].neighbours[ (c ^ 2) & 3 ];
			}
			res++;
		}

	return res;
}

size_t TangleGraph::getCutpoints() const
{
	size_t timer = 1, cutpoint = 0;
	std::vector<size_t> tin(v.size(), 0);
	dfsCP(1, timer, cutpoint, &tin[0]);
	return cutpoint;
}

size_t __fastcall TangleGraph::dfsCP(size_t v, size_t & timer, size_t & cutpoint, size_t tin[]) const
{
	size_t fup = tin[v] = timer++;
	size_t children = 0;
	for(size_t i = 0; i < 4; i++)
	{
		size_t to = TangleGraph::v[v].neighbours[i] >> 2;
		if(to == 0)
			continue;

		if(tin[to] > 0)
		{
			if(tin[to] < fup)
				fup = tin[to];
		}
		else
		{
			children++;
			size_t fup_to = dfsCP(to, timer, cutpoint, tin);
			if(fup_to < fup)
				fup = fup_to;

			if(fup_to >= tin[v])
				cutpoint |= (1 << v);
		}
	}

	if(v == 1)
	{
		if(children > 1)
			cutpoint |= (1 << v);
		else
			cutpoint &= ~(1 << v);
	}

	return fup;
}
