#include <cstring>
#include "IncrementalTangleTemplate.h"

using namespace Tangles;

IncrementalTangleTemplate::IncrementalTangleTemplate()
{
}

IncrementalTangleTemplate::~IncrementalTangleTemplate()
{
}

bool IncrementalTangleTemplate::preCheck(const size_t legsToGlue, const LegIndex posToGlue)
{
	if(!IncrementalTangleProjection::preCheck(legsToGlue, posToGlue))
		return false;

	if(numberOfLegs() == 4 && numberOfCrossings() > 1)
		return false;

	{
		LegIndex l = posToGlue;
		for(size_t i = 1; i < legsToGlue; i++, l = nextLegCW(l))
			if(leg(l).vertex == leg(nextLegCW(l)).vertex)
				return false;
	}

	return true;
}

bool IncrementalTangleTemplate::postCheck(size_t legsToGlue)
{
	if(!IncrementalTangleProjection::postCheck(legsToGlue))
		return false;

	if(legsToGlue == 3 && !flowTest(lastCrossing()))
		return false;

	return true;
}

bool IncrementalTangleTemplate::flowTest(const size_t start) const
{
	const size_t size = 4 * (numberOfCrossings() + 1);

	Util::AutoArray<size_t> memory(size + 2 * numberOfCrossings() + 1);

	int * flow = reinterpret_cast<int *>(memory.get());
	memset(flow, 0, sizeof(int[size]));

	size_t flow_val = 0;
	for(size_t i = 0; i < 4; i++)
		if(neighbour(start, i).vertex == 0)
		{
			flow[4 * start + i]--;
			flow_val++;
		}

	size_t * queue = memory.get() + size, * prev = memory.get() + size + numberOfCrossings();

	const size_t big_edge = (numberOfLegs() == 4) ? 2 : numberOfLegs();
	while(true)
	{
		memset(prev, 0xFF, sizeof(size_t[numberOfCrossings() + 1]));
		size_t head = 0, tail = 0;

		{
			LegIndex l = baseLeg();
			for(size_t i = 0; i < numberOfLegs(); i++, l = nextLegCCW(l))
			{
				size_t next = leg(l).vertex;
				if(prev[next] < 4 || (i != big_edge && flow[leg(l).offset()] != 0))
					continue;

				prev[next] = leg(l).place;
				queue[tail++] = next;
			}
		}

		while(head < tail)
		{
			size_t u = queue[head++];
			if(u == start)
				break;

			for(size_t i = 0; i < 4; i++)
			{
				size_t udir = 4 * u + i;
				size_t v = neighbour(u, i).vertex;
				if(v == 0 || prev[v] < 4 || flow[udir] > 0)
					continue;

				prev[v] = neighbour(u, i).place;
				queue[tail++] = v;
			}
		}

		if(prev[start] >= 4)
			break;

		flow_val++;
		for(size_t i = start; i != 0; i = neighbour(i, prev[i]).vertex)
		{
			flow[4 * i + prev[i]]--;
			flow[neighbour(i, prev[i]).offset()]++;
		}
	}

	if(flow_val < 4)
		return false;

	for(size_t i = 0; i < 4; i++)
	{
		size_t v = neighbour(start, i).vertex;
		if(v != 0 && prev[v] >= 4)
			return false;
	}
	return true;
}
