#pragma once

#include <cstddef>
#include <vector>
#include "../Dart.h"
#include "../Crossings/ArbitraryCrossing.h"

namespace Tangles
{
namespace Impl
{
	class SelfWrithe
	{
	public:
		template<template<typename> class Tangle>
		static int selfWrithe(const Tangle<ArbitraryCrossing> & tangle)
		{
			int writhe = 0;
			size_t stringId = 1;
			std::vector<size_t> visited(4 * (tangle.numberOfCrossings() + 1));

			{
				auto l = tangle.baseLeg();
				for(size_t i = 0; i < tangle.numberOfLegs(); i++, l = l.nextCCW())
					writhe += walkWrithe(tangle, l.leg(), stringId++, visited);
			}

			for(size_t i = 1; i <= tangle.numberOfCrossings(); i++)
				for(size_t j = 0; j < 4; j++)
					writhe += walkWrithe(tangle, Dart(j, i), stringId++, visited);

			return writhe;
		}

	private:
		template<template<typename> class Tangle>
		static int walkWrithe(const Tangle<ArbitraryCrossing> & tangle, const Dart start, const size_t stringId, std::vector<size_t> & visited)
		{
			if(visited[start.offset()] != 0 || visited[start.opposite().offset()] != 0)
				return 0;

			int writhe = 0;
			for(Dart cur = start; !cur.onBorder(); )
			{
				const Dart next = cur.opposite();
				auto & v = visited[next.offset()];

				if(v == 0)
				{
					v = stringId;

					const int d = ArbitraryCrossing::passOver(tangle, next) ? 1 : -1;
					if(visited[next.nextCCW().offset()] == stringId)
						writhe += d;
					else if(visited[next.nextCW().offset()] == stringId)
						writhe -= d;
				}
				else
				{
					assert(v == stringId);
					break;
				}

				cur = tangle.neighbour(next);
			}

			return writhe;
		}
	};
}
}
