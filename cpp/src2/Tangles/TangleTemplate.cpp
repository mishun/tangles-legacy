#include <cstring>
#include "TangleTemplate.h"

using namespace Tangles;

bool TangleTemplate::preCheck(const SubTangle & sub, D4Group orientation, size_t glue_pos, size_t glue_edges) const
{
	if(!TangleProjection::preCheck(sub, orientation, glue_pos, glue_edges))
		return false;

	if(legs() == 4 && glue_edges == 2)
	{
		if(sub.axis(orientation))
			return false;

		const size_t a = border(glue_pos);
		const size_t b = border((glue_pos + legs() - 1) % legs());
		if((a >> 2) == (b >> 2) && vertex(a >> 2).axis(D4Group(b & 3, false)))
			return false;

		if(!(subtangles() == 1 || axis(D4Group(glue_pos, true))))
			return false;
	}
	else
	{
		if(subtangles() > 1 && legs() == 4)
			return false;

		for(size_t i = 0; i + 1 < glue_edges; i++)
		{
			size_t a = border((glue_pos + legs() - i) % legs());
			size_t b = border((glue_pos + legs() - i - 1) % legs());
			if((a >> 2) == (b >> 2))
				return false;
		}
	}

	return true;
}

bool TangleTemplate::postCheck() const
{
	if(!TangleProjection::postCheck())
		return false;

	if(legs() == 4 && glue_edges == 2)
		return true;

	return flowTest(last());
}

bool TangleTemplate::flowTest(const size_t start) const
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

	for(size_t i = 0; i < 4; i++)
	{
		size_t v = neighbour(start, i) >> 2;
		if(v != 0 && prev[v] >= 4)
			return false;
	}
	return true;
}
