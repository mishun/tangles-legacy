#pragma once

#include <vector>
#include "./IncrementalCanonicalTangle.h"

namespace Tangles
{
	template<typename C>
	class IncrementalTemplateTangle : public IncrementalCanonicalTangle<C>
	{
	public:
		using IncrementalTangle<C>::numberOfCrossings;
		using IncrementalTangle<C>::numberOfLegs;
		using IncrementalTangle<C>::neighbour;
		using IncrementalTangle<C>::baseLeg;

	public:
		IncrementalTemplateTangle(const C & crossing)
			: IncrementalCanonicalTangle<C>(crossing)
		{
		}

		virtual ~IncrementalTemplateTangle()
		{
		}

	protected:
		virtual bool preCheck(typename IncrementalTangle<C>::LegIterator const & posToGlue, size_t legsToGlue, const Algebra::D4Group &, const C &) //override
		{
			if(!IncrementalCanonicalTangle<C>::preCheck(posToGlue, legsToGlue))
				return false;

			if(numberOfLegs() == 4 && numberOfCrossings() > 1)
				return false;

			{
				auto l = posToGlue;
				for(size_t i = 1; i < legsToGlue; i++, l = l.nextCW())
					if(l.leg().crossing == l.nextCW().leg().crossing)
						return false;
			}

			return true;
		}

		virtual bool postCheck(size_t lastCrossing, size_t legsToGlue) //override
		{
			if(!IncrementalCanonicalTangle<C>::postCheck(lastCrossing, legsToGlue))
				return false;

			if(legsToGlue == 3 && !flowTest(lastCrossing))
				return false;

			return true;
		}

	private:
		bool flowTest(const size_t start) const
		{
			const size_t size = 4 * (numberOfCrossings() + 1);

			std::vector<int> flow(size, 0);

			size_t flow_val = 0;
			for(size_t i = 0; i < 4; i++)
				if(neighbour(Dart(i, start)).onBorder())
				{
					flow[4 * start + i]--;
					flow_val++;
				}

			std::vector<size_t> queue(numberOfCrossings());
			std::vector<size_t> prev(numberOfCrossings() + 1, 0);

			const size_t big_edge = (numberOfLegs() == 4) ? 2 : numberOfLegs();
			while(true)
			{
				std::memset(&prev.front(), 0xFF, sizeof(size_t) * (numberOfCrossings() + 1));
				size_t head = 0, tail = 0;

				{
					auto l = baseLeg();
					for(size_t i = 0; i < numberOfLegs(); i++, l = l.nextCCW())
					{
						size_t next = l.leg().crossing;
						if(prev[next] < 4 || (i != big_edge && flow[l.leg().offset()] != 0))
							continue;

						prev[next] = l.leg().place;
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
						size_t v = neighbour(Dart(i, u)).crossing;
						if(v == 0 || prev[v] < 4 || flow[udir] > 0)
							continue;

						prev[v] = neighbour(Dart(i, u)).place;
						queue[tail++] = v;
					}
				}

				if(prev[start] >= 4)
					break;

				flow_val++;
				for(size_t i = start; i != 0; i = neighbour(Dart(prev[i], i)).crossing)
				{
					flow[4 * i + prev[i]]--;
					flow[neighbour(Dart(prev[i], i)).offset()]++;
				}
			}

			if(flow_val < 4)
				return false;

			for(size_t i = 0; i < 4; i++)
			{
				size_t v = neighbour(Dart(i, start)).crossing;
				if(v != 0 && prev[v] >= 4)
					return false;
			}

			return true;
		}
	};
}
