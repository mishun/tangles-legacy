#pragma once

#include <cassert>
#include <algorithm>
#include <utility>
#include <vector>
#include <Algebra/Polynomial.h>
#include "./Crossings/ArbitraryCrossing.h"
#include "./TangleUtil.h"

namespace Tangles
{
	class JonesPolynomial
	{
	public:
		template<template<typename> class Tangle>
		static std::pair< Algebra::Polynomial<int>, Algebra::Polynomial<int> > jonesPolynomial(const Tangle<ArbitraryCrossing> & tangle)
		{
			assert(tangle.numberOfLegs() == 4);
			using Algebra::Polynomial;

			const Polynomial<int> a = Polynomial<int>::x(1);
			const Polynomial<int> b = Polynomial<int>::invx(1);
			const Polynomial<int> d = -a * a - b * b;

			std::pair< Polynomial<int>, Polynomial<int> > jones(Polynomial<int>::constant(0), Polynomial<int>::constant(0));

			for(size_t mask = 0; mask < (1U << tangle.numberOfCrossings()); mask++)
			{
				Polynomial<int> cur = Polynomial<int>::constant(1);

				for(size_t i = 0; i < tangle.numberOfCrossings(); i++)
					if(mask & (1 << i))
						cur = cur * b;
					else
						cur = cur * a;

				std::vector<bool> visited(4 * (tangle.numberOfCrossings() + 1), false);
				std::vector<size_t> cd(tangle.numberOfLegs(), 0);

				{
					auto l = tangle.baseLeg();
					for(size_t i = 0; i < tangle.numberOfLegs(); i++, l = l.nextCCW())
					{
						Dart f = walk(l.leg(), tangle, mask, visited);

						for(auto t = l; t.leg() != f; t = t.nextCCW())
							cd[i]++;
					}
				}

				for(size_t i = 1; i <= tangle.numberOfCrossings(); i++)
					for(size_t j = 0; j < 4; j++)
					{
						Dart s(j, i);
						if(!visited[s.offset()])
						{
							Dart f = walk(s, tangle, mask, visited);
							assert(s == f);
							cur = cur * d;
						}
					}

				assert(cd[0] == 1 || cd[0] == tangle.numberOfLegs() - 1);
				if(cd[0] == 1)
					jones.first = jones.first + cur;
				else
					jones.second = jones.second + cur;
			}

			{
				auto w = writheMultiplier(tangle);
				jones.first = jones.first * w;
				jones.second = jones.second * w;
			}

			return jones;
		}

	private:
		template<template<typename> class Tangle>
		static Dart walk(const Dart start, const Tangle<ArbitraryCrossing> & tangle, const size_t mask, std::vector<bool> & visited)
		{
			for(Dart cur = start; ;)
			{
				if(visited[cur.offset()])
					return cur;
				else
					visited[cur.offset()] = true;

				const Dart next = connect(cur, tangle, mask);
				assert(!visited[next.offset()]);
				visited[next.offset()] = true;

				cur = tangle.neighbour(next);

				if(cur.onBorder())
					return next;
			}
		}

		template<template<typename> class Tangle>
		static Dart connect(const Dart d, const Tangle<ArbitraryCrossing> & tangle, const size_t mask)
		{
			assert(!d.onBorder());

			const bool type = (mask & (1 << (d.crossing - 1))) != 0;
			const bool over = ArbitraryCrossing::passOver(tangle, d);

			if(over == type)
				return d.nextCCW();
			else
				return d.nextCW();
		}

		template<template<typename> class Tangle>
		static Algebra::Polynomial<int> writheMultiplier(const Tangle<ArbitraryCrossing> & tangle)
		{
			int writhe = selfWrithe(tangle);
			size_t absWrithe = static_cast<size_t>(abs(writhe));

			if(writhe >= 0)
				return Algebra::Polynomial<int>::invx(-1).power(3 * absWrithe);
			else
				return Algebra::Polynomial<int>::x(-1).power(3 * absWrithe);
		}
	};
}
