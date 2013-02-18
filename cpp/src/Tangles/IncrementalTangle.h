#pragma once

#include <cassert>
#include <cstddef>
#include "./Dart.h"
#include <Algebra/D4Group.h>
#include <Algebra/DnSubGroup.h>

namespace Tangles
{
	template<typename Crossing>
	class IncrementalTangle
	{
	private:
		struct VertexData
		{
			Dart neighbours[4];
			Algebra::D4Group rotation;
			Crossing crossing;
		};

		struct LegData
		{
			Dart leg;
			size_t tmp : 24;
			LegData * next;
			LegData * prev;
		};

		struct State
		{
			size_t legsToGlue;
			LegData * baseLeg;
			LegData * glueBegin;
			LegData * glueEnd;
			LegData * newBegin;
			LegData * newEnd;
		};

	public:
		class LegIterator
		{
			friend class IncrementalTangle;

		private:
			LegData * legData;

		private:
			LegIterator(LegData * ld)
				: legData(ld)
			{
			}

		public:
			typedef Crossing CrossingType;

			LegIterator(const LegIterator & that)
				: legData(that.legData)
			{
			}

			const LegIterator & operator=(const LegIterator & that)
			{
				legData = that.legData;
				return *this;
			}

			LegIterator nextCCW() const
			{
				return LegIterator(legData->next);
			}

			LegIterator nextCW() const
			{
				return LegIterator(legData->prev);
			}

			LegIterator nextCCW(size_t times) const
			{
				LegIterator result(*this);
				for(size_t i = 0; i < times; i++)
					result = result.nextCCW();
				return result;
			}

			LegIterator nextCW(size_t times) const
			{
				LegIterator result(*this);
				for(size_t i = 0; i < times; i++)
					result = result.nextCW();
				return result;
			}

			Dart leg() const
			{
				return legData->leg;
			}

			bool operator==(LegIterator that) const
			{
				return legData == that.legData;
			}

			bool operator!=(LegIterator that) const
			{
				return legData != that.legData;
			}
		};

	public:
		static const size_t maxStackDepth = 16;

	protected:
		size_t stackDepth;

	private:
		size_t legs;
		LegData * _baseLeg;
		LegData * freeLeg;

		VertexData v[maxStackDepth];
		LegData l[4 * maxStackDepth];
		State stack[maxStackDepth];

	public:
		IncrementalTangle(const Crossing & crossing)
			: stackDepth(0)
			, legs(4)
			, _baseLeg(l)
			, freeLeg(l + 4)
		{
			v[1].rotation = Algebra::D4Group::I;
			v[1].crossing = crossing;

			for(size_t j = 0; j < 4; j++)
			{
				v[1].neighbours[j] = Dart(0, 0);

				LegData & leg = l[j];
				leg.next = &l[(j + 1) & 3];
				leg.prev = &l[(j - 1) & 3];

				leg.leg = Dart(j, 1); 
			}
		}

		virtual ~IncrementalTangle()
		{
		}

		size_t numberOfCrossings() const
		{
			return stackDepth + 1;
		}

		size_t numberOfLegs() const
		{
			return legs;
		}

		size_t numberOfEdges() const
		{
			return 2 * numberOfCrossings() + numberOfLegs() / 2;
		}

		Dart neighbour(Dart dart) const
		{
			assert(dart.crossing > 0 && dart.crossing <= numberOfCrossings());
			return v[dart.crossing].neighbours[dart.place];
		}

		const Algebra::D4Group & rotation(size_t index) const
		{
			assert(index > 0 && index <= numberOfCrossings());
			return v[index].rotation;
		}

		const Crossing & crossing(size_t index) const
		{
			assert(index > 0 && index <= numberOfCrossings());
			return v[index].crossing;
		}

		LegIterator baseLeg() const
		{
			return LegIterator(_baseLeg);
		}

		//       edgesToGlue = 1                 edgesToGlue = 2                 edgesToGlue = 3
		// ........|                       ........|                       ........|
		// (pos+1)-|---------------3       (pos+1)-|---------------2       (pos+1)-|---------------1
		//         |  +=========+                  |  +=========+                  |  +=========+
		//  (pos)--|--|-0-\ /-3-|--2        (pos)--|--|-0-\ /-3-|--1        (pos)--|--|-0-\ /-3-|--0
		// ........|  |    *    |                  |  |    *    |                  |  |    *    |
		// ........|  |   / \-2-|--1       (pos-1)-|--|-1-/ \-2-|--0       (pos-1)-|--|-1-/ \   |
		// ........|  |  1      |          ........|  +=========+                  |  |      2  |
		// ........|  |   \-----|--0       ........|                       (pos-2)-|--|-----/   |
		// ........|  +=========+          ........|                       ........|  +=========+
		bool glue(const LegIterator & positionToGlue, const size_t legsToGlue, const Algebra::D4Group & rotation, const Crossing & crossing)
		{
			assert(stackDepth + 1 < maxStackDepth);
			assert(legsToGlue >= 1 && legsToGlue <= 3);
			assert(legsToGlue < numberOfLegs());

			if(!preCheck(positionToGlue, legsToGlue, rotation, crossing))
				return false;

			const size_t newVertexIndex = stackDepth + 2;

			{
				auto & newVertex = v[newVertexIndex];
				newVertex.rotation = rotation;
				newVertex.crossing = crossing;

				auto * glueBegin = positionToGlue.legData;
				auto * glueEnd = glueBegin;

				{
					auto * j = glueEnd;
					for(size_t i = 0; i < legsToGlue; i++, j = j->prev)
					{
						glueBegin = j;
						const Dart l = j->leg;

						newVertex.neighbours[i] = l;
						v[l.crossing].neighbours[l.place] = Dart(i, newVertexIndex);
					}
				}

				auto * newBegin = freeLeg;
				auto * newEnd = freeLeg;

				for(size_t i = legsToGlue; i < 4; i++)
				{
					newEnd = freeLeg;

					newEnd->next = freeLeg + 1;
					newEnd->prev = freeLeg - 1;
					newEnd->leg = Dart(i, newVertexIndex);
					newVertex.neighbours[i] = Dart(0, 0);

					freeLeg++;
				}

				{
					auto & state = stack[stackDepth++];
					state.legsToGlue = legsToGlue;
					state.baseLeg = _baseLeg;

					state.glueBegin = glueBegin;
					state.glueEnd = glueEnd;
					state.newBegin = newBegin;
					state.newEnd = newEnd;
				}

				replaceLegsRange(glueBegin, glueEnd, newBegin, newEnd);
				_baseLeg = newBegin;
				legs += 4 - 2 * legsToGlue;
			}

			if(!postCheck(newVertexIndex, legsToGlue))
			{
				cancell();
				return false;
			}

			return true;
		}

		void cancell()
		{
			assert(stackDepth > 0);

			auto & state = stack[--stackDepth];

			replaceLegsRange(state.newBegin, state.newEnd, state.glueBegin, state.glueEnd);
			for(LegData * i = state.glueBegin; ; i = i->next)
			{
				const Dart l = i->leg;
				v[l.crossing].neighbours[l.place] = Dart(0, 0);

				if(i == state.glueEnd)
					break;
			}

			freeLeg -= 4 - state.legsToGlue;
			legs += 2 * state.legsToGlue - 4;
			_baseLeg = state.baseLeg;
		}

	public:
		virtual const Algebra::DnSubGroup & symmetry() const = 0;

	protected:
		virtual bool preCheck(const LegIterator &, size_t, const Algebra::D4Group &, const Crossing &) = 0;
		virtual bool postCheck(size_t lastCrossing, size_t legsToGlue) = 0;

	private:
		void replaceLegsRange(LegData * fromBegin, LegData * fromEnd, LegData * toBegin, LegData * toEnd)
		{
			LegData * prev = fromBegin->prev;
			LegData * next = fromEnd->next;

			prev->next = toBegin;
			toBegin->prev = prev;
			toEnd->next = next;
			next->prev = toEnd;
		}

	private:
		IncrementalTangle(const IncrementalTangle &);
		const IncrementalTangle & operator=(const IncrementalTangle &);
	};
}
