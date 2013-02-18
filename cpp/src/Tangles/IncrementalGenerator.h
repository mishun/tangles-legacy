#pragma once

#include <cassert>
#include <ctime>
#include <set>
#include <Algebra/D4Group.h>
#include <Algebra/D4SubGroup.h>
#include "./IncrementalTangle.h"
#include "./TangleUtil.h"

namespace Tangles
{
	template<template<typename> class T, class C, const bool triangle = false>
	class IncrementalGenerator
	{
	public:
		const size_t maxCrossings;

	private:
		unsigned long long _total;
		double _totalTime;

	public:
		IncrementalGenerator(size_t mc)
			: maxCrossings(mc)
			, _total(0)
		{
		}

		virtual ~IncrementalGenerator()
		{
		}

		unsigned long long total() const
		{
			return _total;
		}

		double time() const
		{
			return _totalTime;
		}

		unsigned long long generate()
		{
			_total = 0;
			init();

			const double beginTime = clock();

			{
				C c;
				T<C> tangle(c);
				incrementalBacktrack(tangle);
			}

			_totalTime = (clock() - beginTime) / CLOCKS_PER_SEC;

			done();
			return _total;
		}

	private:
		void incrementalBacktrack(IncrementalTangle<C> & tangle)
		{
			using Algebra::D4Group;
			using Algebra::D4SubGroup;

			{
				_total++;
				const bool processChildren = visit(tangle);
				if(!processChildren || tangle.numberOfCrossings() >= maxCrossings)
					return;
			}

			C crossing;

			const auto & tangleSymmetry = tangle.symmetry();
			const auto & crossingSymmetry = crossing.symmetry();
			const auto tanglePeriod = tangleSymmetry.period();

			if(tangleSymmetry.mirror())
			{
				for(size_t gl = 3; gl >= 1; gl--)
				{
					if(triangle && tangle.numberOfCrossings() + tangle.numberOfLegs() / 2 - gl >= maxCrossings)
						continue;

					const size_t mirrorDiff = (tangleSymmetry.mirrorDiff() + gl - 1) % tanglePeriod;
					const auto left = tangle.baseLeg().nextCW((tanglePeriod - mirrorDiff) / 2);
					const auto right = tangle.baseLeg().nextCCW(1 + mirrorDiff / 2);

					for(auto l = left; l != right; l = l.nextCCW())
					{
						std::set< Data::SharedArray<size_t> > s;
						crossingSymmetry.forEachClassRepresentative(
							[this, &tangle, &l, &gl, &crossing, &s, &left, &right](const Algebra::D4Group & r) -> void
							{
								if(tangle.glue(l, gl, r, crossing))
								{
									auto inv = diskHomeomorphismInvariant(tangle);
									if(s.count(inv) == 0)
									{
										this->incrementalBacktrack(tangle);
										if(l == left || l.nextCCW() == right)
											s.insert(inv);
									}
									tangle.cancell();
								}
							}
						);
					}
				}
			}
			else
				for(size_t gl = 3; gl >= 1; gl--)
				{
					if(triangle && tangle.numberOfCrossings() + tangle.numberOfLegs() / 2 - gl >= maxCrossings)
						continue;

					auto l = tangle.baseLeg();
					for(size_t i = 0; i < tanglePeriod; i++, l = l.nextCCW())
						crossingSymmetry.forEachClassRepresentative(
							[this, &tangle, gl, &l, &crossing](const Algebra::D4Group & r) -> void
							{
								if(tangle.glue(l, gl, r, crossing))
								{
									this->incrementalBacktrack(tangle);
									tangle.cancell();
								}
							}
						);
				}
		}

	protected:
		virtual bool visit(const IncrementalTangle<C> &) = 0;

		virtual void init()
		{
		}

		virtual void done()
		{
		}
	};
}
