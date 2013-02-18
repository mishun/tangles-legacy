#pragma once

#include <Topology/EmbeddedGraph.h>
#include <Data/SharedArray.h>
#include <Algebra/D4Group.h>
#include "./Dart.h"
#include "./Crossings/ArbitraryCrossing.h"
#include "./Impl/SelfWrithe.h"
#include "./Impl/DiskHomeomorphismInvariant.h"
#include "./Impl/ShortInvariant.h"

namespace Tangles
{
	template<template<typename C> class Tangle, typename C>
	inline Data::SharedArray<size_t> diskHomeomorphismInvariant(const Tangle<C> & tangle)
	{
		return Impl::DiskHomeomorphismInvariant::invariant(tangle);
	}

	template<template<typename C> class Tangle, typename C>
	inline Data::SharedArray<size_t> shortInvariant(const Tangle<C> & tangle)
	{
		return Impl::ShortInvariant::invariant(tangle);
	}

	template<class Tangle>
	inline bool connected(const Tangle & tangle)
	{
		std::vector<bool> flag(tangle.numberOfCrossings() + 1, false);
		std::queue<size_t> q;

		flag[1] = true;
		q.push(1);

		while(!q.empty())
		{
			const size_t v = q.front();
			q.pop();

			for(size_t i = 0; i < 4; i++)
			{
				size_t u = tangle.neighbour(Dart(i, v)).crossing;
				if(u != 0 && !flag[u])
				{
					flag[u] = true;
					q.push(u);
				}
			}
		}

		for(size_t i = 1; i <= tangle.numberOfCrossings(); i++)
			if(!flag[i])
				return false;

		return true;
	}

	template<template<typename> class Tangle>
	inline bool alternating(const Tangle<ArbitraryCrossing> & tangle)
	{
		for(size_t i = 1; i <= tangle.numberOfCrossings(); i++)
			for(size_t j = 0; j < 4; j++)
			{
				const Dart a = Dart(j, i);
				const Dart b = tangle.neighbour(a);

				if(!b.onBorder() && ArbitraryCrossing::passOver(tangle, a) == ArbitraryCrossing::passOver(tangle, b))
					return false;
			}

		return true;
	}

	template<template<typename> class Tangle>
	inline int selfWrithe(const Tangle<ArbitraryCrossing> & tangle)
	{
		return Impl::SelfWrithe::selfWrithe(tangle);
	}

	template<class Tangle>
	inline Topology::EmbeddedGraph & toGraph(const Tangle & tangle)
	{
		std::vector< std::vector< std::pair<size_t, size_t> > > g(tangle.numberOfCrossings() + 1);

		for(size_t i = 1; i <= tangle.numberOfCrossings(); i++)
		{
			g[i].resize(4);
			for(size_t j = 0; j < 4; j++)
			{
				Dart dart(j, i);
				dart = tangle.neighbour(dart);

				if(dart.crossing != 0)
				{
					g[i] [j].first = dart.crossing;
					g[i] [j].second = dart.place;
				}
			}
		}

		g[0].resize(tangle.numberOfLegs());
		{
			auto lid = tangle.baseLeg().nextCW();
			for(size_t i = 0; i < tangle.numberOfLegs(); i++, lid = lid.nextCW())
			{
				const Dart l = lid.leg();

				g[0] [i].first = l.crossing;
				g[0] [i].second = l.place;
				g[l.crossing] [l.place] = std::make_pair(0, i);
			}
		}

		return Topology::EmbeddedGraph::createFromVertexAdjacencyList(g);
	}
}
