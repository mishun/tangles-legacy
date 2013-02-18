#pragma once

#include <vector>
#include <queue>
#include <Data/SharedArray.h>
#include "../Dart.h"
#include "../Crossings/ArbitraryCrossing.h"

namespace Tangles
{
namespace Impl
{
	class DiskHomeomorphismInvariant
	{
	private:
		template<class Tangle>
		static int tryRootCode(const Tangle & tangle, size_t rcode[], const Dart start, const int dir, const Algebra::D4Group & global)
		{
			using Algebra::D4Group;

			std::vector<size_t> id(tangle.numberOfCrossings() + 1, 0);
			id[start.crossing] = 1;

			size_t tail = 0;
			std::vector<Dart> queue(tangle.numberOfCrossings());
			queue[tail++] = start;

			size_t free_id = 2;
			bool better = false;
			for(size_t head = 0; head < tangle.numberOfCrossings(); head++)
			{
				const Dart base = queue[head];
				size_t vcode = 0;

				Dart it = base;
				for(size_t i = 0; i < 4; i++, it = it.next(dir))
				{
					const Dart neigh = tangle.neighbour(it);
					vcode <<= 6;

					if(!neigh.onBorder())
					{
						if(id[neigh.crossing] == 0)
						{
							id[neigh.crossing] = free_id++;
							queue[tail++] = neigh;
						}
						vcode |= id[neigh.crossing];
					}
				}

				vcode = (vcode << 3) | tangle.crossing(base.crossing).symmetry().factorSetId(D4Group(base.place, dir < 0) * tangle.rotation(base.crossing) * global);

				if(better || vcode < rcode[head])
				{
					rcode[head] = vcode;
					better = true;
				}
				else if(vcode > rcode[head])
					return -1;
			}

			if(better)
				return 1;
			return 0;
		}

	public:
		template<template<typename C> class Tangle, typename C>
		static Data::SharedArray<size_t> invariant(const Tangle<C> & tangle)
		{
			Data::SharedArray<size_t> inv(tangle.numberOfCrossings(), 0xFFFFFFFF);
			
			auto l = tangle.baseLeg();
			for(size_t i = 0; i < tangle.numberOfLegs(); i++, l = l.nextCCW())
			{
				const Dart cur = l.leg();
				for(int dir = -1; dir <= 1; dir += 2)
				{
					const auto & gs = C::globalSymmetry();

					bool flag[8] = { false, false, false, false, false, false, false, false };
					for(size_t index = 0; index < 8; index++)
					{
						const Algebra::D4Group r(index & 3, (index & 4) != 0);
						const auto fs = gs.factorSetId(r);
						if(flag[fs])
							continue;
						flag[fs] = true;

						tryRootCode(tangle, inv.get(), cur, dir, r);
					}
				}
			}

			return inv;
		}
	};
}
}
