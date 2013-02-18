#pragma once

#include <cassert>
#include <cstring>
#include <vector>
#include <Algebra/D4Group.h>
#include "./IncrementalTangle.h"

namespace Tangles
{
	template<typename C>
	class IncrementalCanonicalTangle : public IncrementalTangle<C>
	{
	private:
		using IncrementalTangle<C>::maxStackDepth;
		using IncrementalTangle<C>::stackDepth;

	public:
		using typename IncrementalTangle<C>::LegIterator;
		using IncrementalTangle<C>::numberOfCrossings;
		using IncrementalTangle<C>::numberOfLegs;
		using IncrementalTangle<C>::neighbour;
		using IncrementalTangle<C>::crossing;
		using IncrementalTangle<C>::rotation;
		using IncrementalTangle<C>::baseLeg;

	private:
		struct StateDump
		{
			size_t cutpointsMask;
			Algebra::DnSubGroup symmetry;
		};

		struct DfsResult
		{
			size_t fup;
			size_t border;
		};

	private:
		StateDump dumpStack[maxStackDepth];

	public:
		IncrementalCanonicalTangle(const C & crossing)
			: IncrementalTangle<C>(crossing)
		{
			// TODO: symmetry group
		}

		virtual ~IncrementalCanonicalTangle()
		{
		}

		virtual const Algebra::DnSubGroup & symmetry() const //override
		{
			return dumpStack[stackDepth].symmetry;
		}

		size_t getCutpoints() const
		{
			return dumpStack[stackDepth].cutpointsMask;
		}

	protected:
		virtual bool preCheck(typename IncrementalTangle<C>::LegIterator const &, size_t legsToGlue, const Algebra::D4Group &, const C &) //override
		{
			if(numberOfLegs() + 4 - 2 * legsToGlue < 4)
				return false;

			return true;
		}

		virtual bool postCheck(size_t lastCrossing, size_t legsToGlue) //override
		{
			using Algebra::DnSubGroup;

			const auto cutpointsMask = getCutpointsMask(lastCrossing, legsToGlue);
			if((cutpointsMask & 1) != 0)
				return false;

			std::vector<size_t> rcode(2 * numberOfCrossings(), 0xFFFFFFFF);

			tryRootCode(&rcode.front(), Dart(legsToGlue, lastCrossing), 1);
			tryRootCode(&rcode.front(), Dart(3, lastCrossing), -1);

			size_t symmetryDir = 0;
			size_t symmetryRev = 0;
			size_t positionDir = 0;
			size_t positionRev = 3 - legsToGlue;

			{
				auto l = baseLeg();
				for(size_t i = 0; i < numberOfLegs(); i++, l = l.nextCCW())
				{
					const Dart cur = l.leg();

					if(cutpointsMask & (1 << cur.crossing))
						continue;

					if(cur.crossing != l.nextCCW().leg().crossing)
					{
						int compareResult = tryRootCode(&rcode.front(), cur, -1);

						if(compareResult > 0)
							return false;

						if(compareResult == 0)
						{
							symmetryRev++;
							positionRev = i;
						}
					}

					if(cur.crossing != l.nextCW().leg().crossing)
					{
						int compareResult = tryRootCode(&rcode.front(), cur, 1);

						if(compareResult > 0)
							return false;

						if(compareResult == 0)
						{
							symmetryDir++;
							positionDir = i;
						}
					}
				}
			}

			assert(symmetryDir == 0 || symmetryRev == 0 || symmetryDir == symmetryRev);

			{
				StateDump & stateDump = dumpStack[stackDepth];
				stateDump.cutpointsMask = cutpointsMask;

				const size_t period = numberOfLegs() / std::max(symmetryDir, symmetryRev);
				if(symmetryDir == symmetryRev)
					stateDump.symmetry = DnSubGroup::createFromPeriodAndMirroredZero(numberOfLegs(), period, positionRev + positionDir);
				else
					stateDump.symmetry = DnSubGroup::createFromPeriod(numberOfLegs(), period);
			}

			return true;
		}

	private:
		size_t getCutpointsMask(const size_t lastCrossing, const size_t legsToGlue) const
		{
			size_t cutpointsMask = 0;
			if(legsToGlue == 1)
			{
				if(numberOfCrossings() > 2)
					cutpointsMask = dumpStack[stackDepth - 1].cutpointsMask | (1 << neighbour(Dart(0, lastCrossing)).crossing);
			}
			else
			{
				size_t timer = 1, cutpoints = 0;
				std::vector<size_t> tin(numberOfCrossings() + 1, 0);

				const DfsResult result = dfs(lastCrossing, 0, timer, cutpoints, &tin.front());
				if(legsToGlue == 3 && result.border == 1)
					return 1;

				cutpointsMask = cutpoints & ~(1 << lastCrossing);
			}

			return cutpointsMask;
		}

		DfsResult dfs(const size_t v, const size_t from, size_t & timer, size_t & cutpoint, size_t tin[]) const
		{
			DfsResult result;
			result.fup = tin[v] = timer++;
			result.border = 0;

			for(size_t i = 0; i < 4; i++)
			{
				const size_t to = neighbour(Dart(i, v)).crossing;

				if(to == from)
					continue;

				if(to == 0)
				{
					result.border++;
					continue;
				}

				if(tin[to] > 0)
				{
					if(tin[to] < result.fup)
						result.fup = tin[to];
				}
				else
				{
					const DfsResult that = dfs(to, v, timer, cutpoint, tin);

					if(that.fup < result.fup)
						result.fup = that.fup;

					if(that.fup >= tin[v])
						cutpoint |= (1 << v);

					if(that.fup <= tin[v])
						result.border += that.border;
					else
						result.border++;
				}
			}

			return result;
		}

		int tryRootCode(size_t rcode[], const Dart start, const int dir, const Algebra::D4Group & global) const
		{
			using Algebra::D4Group;

			std::vector<size_t> id(numberOfCrossings() + 1, 0);
			id[start.crossing] = 1;

			size_t tail = 0;
			std::vector<Dart> queue(numberOfCrossings());
			queue[tail++] = start;

			size_t free_id = 2;
			bool better = false;
			for(size_t head = 0; head < numberOfCrossings(); head++)
			{
				const Dart base = queue[head];
				size_t vcode = 0;

				Dart it = base;
				for(size_t i = 0; i < 4; i++, it = it.next(dir))
				{
					const Dart neigh = neighbour(it);
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

				vcode = (vcode << 3) | crossing(base.crossing).symmetry().factorSetId(D4Group(base.place, dir < 0) * rotation(base.crossing) * global);

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

		int tryRootCode(size_t rcode[], const Dart start, const int dir) const
		{
			int result = -1;
			const auto & gs = C::globalSymmetry();

			bool flag[8] = { false, false, false, false, false, false, false, false };
			for(size_t index = 0; index < 8; index++)
			{
				const Algebra::D4Group r(index & 3, (index & 4) != 0);
				const auto fs = gs.factorSetId(r);
				if(flag[fs])
					continue;
				flag[fs] = true;

				int current = tryRootCode(rcode, start, dir, r);

				if(current > result)
					result = current;
			}

			return result;
		}
	};
}
