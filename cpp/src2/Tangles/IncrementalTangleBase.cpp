#include <cassert>
#include "IncrementalTangleBase.h"

using namespace Tangles;

IncrementalTangleBase::IncrementalTangleBase()
	: stackDepth(0)
	, vertexes(1)
	, legs(4)
	, base_leg(0)
	, free_leg(4)
{
	for(size_t i = 0; i < 4; i++)
	{
		v[1].neighbours[i].vertex = 0;
		v[1].neighbours[i].place = 0;

		Leg & cur = l[i];
		cur.nextCCW = LegIndex((i + 1) & 3);
		cur.nextCW = LegIndex((i - 1) & 3);
		cur.leg.vertex = 1;
		cur.leg.place = i;
	}
}

IncrementalTangleBase::~IncrementalTangleBase()
{
}

bool IncrementalTangleBase::glue(const size_t legsToGlue, const LegIndex posToGlue)
{
	assert(legsToGlue >= 1 && legsToGlue <= 3);
	assert(legsToGlue < numberOfLegs());

	if(!preCheck(legsToGlue, posToGlue))
		return false;

	vertexes++;
	Vertex & newVertex = v[vertexes];

	LegIndex glueEnd = posToGlue;
	LegIndex glueBegin = posToGlue;

	{
		LegIndex j = posToGlue;
		for(size_t i = 0; i < legsToGlue; i++, j = nextLegCW(j))
		{
			glueBegin = j;
			const Dart l = leg(j);

			newVertex.neighbours[i] = l;
			v[l.vertex].neighbours[l.place].place = i;
			v[l.vertex].neighbours[l.place].vertex = vertexes;
		}
	}

	LegIndex newBegin(free_leg);
	LegIndex newEnd(free_leg);

	for(size_t i = legsToGlue; i < 4; i++)
	{
		newEnd.index = free_leg++;
		Leg & leg = l[newEnd.index];

		leg.nextCCW = LegIndex(newEnd.index + 1);
		leg.nextCW = LegIndex(newEnd.index - 1);
		leg.leg.place = i;
		leg.leg.vertex = vertexes;

		newVertex.neighbours[i].place = 0;
		newVertex.neighbours[i].vertex = 0;
	}

	{
		StateDump & stateDump = dumpStack[stackDepth++];

		stateDump.legsGlued = legsToGlue;
		stateDump.baseLeg = base_leg;
		stateDump.glueBegin = glueBegin;
		stateDump.glueEnd = glueEnd;
		stateDump.newBegin = newBegin;
		stateDump.newEnd = newEnd;
	}

	replaceLegsRange(glueBegin, glueEnd, newBegin, newEnd);
	base_leg = newBegin;
	legs += 4 - 2 * legsToGlue;

	if(!postCheck(legsToGlue))
	{
		cancellGlue();
		return false;
	}

	return true;
}

void IncrementalTangleBase::cancellGlue()
{
	assert(vertexes > 1);

	StateDump & stateDump = dumpStack[--stackDepth];

	const LegIndex glueBegin = stateDump.glueBegin;
	const LegIndex glueEnd = stateDump.glueEnd;
	const LegIndex newBegin = stateDump.newBegin;
	const LegIndex newEnd = stateDump.newEnd;

	replaceLegsRange(newBegin, newEnd, glueBegin, glueEnd);

	for(LegIndex i = glueBegin; ; i = nextLegCCW(i))
	{
		const Dart l = leg(i);
		v[l.vertex].neighbours[l.place].place = 0;
		v[l.vertex].neighbours[l.place].vertex = 0;

		if(i == glueEnd)
			break;
	}

	vertexes--;
	free_leg -= 4 - stateDump.legsGlued;
	legs += 2 * stateDump.legsGlued - 4;
	base_leg = stateDump.baseLeg;
}

Topology::EmbeddedGraph & IncrementalTangleBase::toGraph() const
{
	std::vector< std::vector< std::pair<size_t, size_t> > > g(numberOfCrossings() + 1);

	for(size_t i = 1; i <= numberOfCrossings(); i++)
	{
		g[i].resize(4);
		for(size_t j = 0; j < 4; j++)
		{
			Dart dart;
			dart.place = j;
			dart.vertex = i;
			dart = neighbour(dart);

			if(dart.vertex != 0)
			{
				g[i] [j].first = dart.vertex;
				g[i] [j].second = dart.place;
			}
		}
	}

	g[0].resize(numberOfLegs());
	{
		LegIndex lid = nextLegCW(baseLeg());
		for(size_t i = 0; i < numberOfLegs(); i++, lid = nextLegCW(lid))
		{
			const Dart l = leg(lid);

			g[0] [i].first = l.vertex;
			g[0] [i].second = l.place;
			g[l.vertex] [l.place] = std::make_pair(0, i);
		}
	}

	return Topology::EmbeddedGraph::createFromVertexAdjacencyList(g);
}
