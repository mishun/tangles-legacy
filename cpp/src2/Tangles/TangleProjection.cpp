#include <cstring>
#include <iostream>
#include <memory>
#include <vector>
#include <Util/Util.h>
#include "TangleProjection.h"

using namespace Tangles;

TangleGraph TangleProjection::getSimpleGraph() const
{
	TangleGraph res(subtangles(), legs());

	for(size_t i = 1; i <= subtangles(); i++)
		for(size_t j = 0; j < 4; j++)
			res.v[i].neighbours[j] = neighbour(i, j);

	for(size_t i = 0; i < legs(); i++)
		res.l[i] = border(i);

	return res;
}

TangleGraph TangleProjection::getGraph() const
{
	TangleGraph res = getSimpleGraph();
	std::vector<TangleGraph> st(subtangles() + 1);

	for(size_t i = 1; i <= subtangles(); i++)
		st[i] = vertex(i).getSubGraph();

	return res.substitute(st);
}

bool TangleProjection::preCheck(const SubTangle & sub, D4Group orientation, size_t glue_pos, size_t glue_edges) const
{
	if(!TangleWithRootCode::preCheck(sub, orientation, glue_pos, glue_edges))
		return false;

	return true;
}

bool TangleProjection::postCheck() const
{
	if(!TangleWithRootCode::postCheck())
		return false;

	return primeTest(last());
}

bool TangleProjection::primeTest(const size_t start) const
{
	if(glue_edges < 3)
		return true;

	const size_t size = 4 * (subtangles() + 1);

	Util::AutoArray<size_t> memory(size + 2 * subtangles() + 1);

	int * flow = reinterpret_cast<int *>(memory.get());
	std::memset(flow, 0, sizeof(int[size]));

	size_t flow_val = 0;
	for(size_t i = 0; i < 4; i++)
		if(neighbour(start, i) == 0)
		{
			flow[4 * start + i]--;
			flow_val++;
		}

	size_t * queue = memory.get() + size, * prev = memory.get() + size + subtangles();

	const size_t big_edge = (legs() == 4) ? 2 : legs();
	while(true)
	{
		std::memset(prev, 0xFF, sizeof(size_t[subtangles() + 1]));
		size_t head = 0, tail = 0;

		for(size_t i = 0; i < legs(); i++)
		{
			size_t next = border(i) >> 2;
			if(prev[next] < 4 || (i != big_edge && flow[border(i)] != 0))
				continue;

			prev[next] = border(i) & 3;
			queue[tail++] = next;
		}

		while(head < tail)
		{
			size_t u = queue[head++];
			if(u == start)
				break;

			for(size_t i = 0; i < 4; i++)
			{
				size_t udir = 4 * u + i;
				size_t v = neighbour(u, i) >> 2;
				if(v == 0 || prev[v] < 4 || flow[udir] > 0)
					continue;

				prev[v] = neighbour(u, i) & 3;
				queue[tail++] = v;
			}
		}

		if(prev[start] >= 4)
			break;

		flow_val++;
		for(size_t i = start; i != 0; i = neighbour(i, prev[i]) >> 2)
		{
			flow[4 * i + prev[i]]--;
			flow[neighbour(i, prev[i])]++;
		}
	}

	if(flow_val < 4)
		return false;

	return true;
}
