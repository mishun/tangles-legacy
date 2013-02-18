#pragma once

#include <cstddef>
#include <Topology/EmbeddedGraph.h>

namespace Tangles
{
	class IncrementalTangleBase
	{
	public:
		static const size_t maxSize = 32;

	public:
		struct Dart
		{
			size_t place : 2;
			size_t vertex : 6;

			size_t offset() const
			{
				return place + (vertex << 2);
			}
		};

		struct LegIndex
		{
			friend class IncrementalTangleBase;

		public:
			bool operator==(LegIndex that) { return index == that.index; }
			bool operator!=(LegIndex that) { return index != that.index; }

		private:
			explicit LegIndex(size_t i = 0) : index(i) {}

		private:
			size_t index : 8;
		};

	private:
		struct Vertex
		{
			Dart neighbours[4];
		};

		struct Leg
		{
			Dart leg;
			LegIndex nextCCW;
			LegIndex nextCW;
			size_t tmp : 8;
		};

		struct StateDump
		{
			size_t legsGlued;
			LegIndex baseLeg;
			LegIndex glueBegin;
			LegIndex glueEnd;
			LegIndex newBegin;
			LegIndex newEnd;
		};

	protected:
		size_t stackDepth;

	private:
		size_t vertexes;
		size_t legs;
		Vertex v[maxSize];
		Leg l[4 * maxSize];
		LegIndex base_leg;
		size_t free_leg;
		StateDump dumpStack[maxSize];

	public:
		size_t numberOfCrossings() const
		{
			return vertexes;
		}

		size_t numberOfEdges() const
		{
			return 2 * vertexes + legs / 2;
		}

		size_t numberOfLegs() const
		{
			return legs;
		}

		size_t lastCrossing() const
		{
			return vertexes;
		}

		Dart neighbour(Dart dart) const
		{
			return v[dart.vertex].neighbours[dart.place];
		}

		Dart neighbour(size_t vertex, size_t place) const
		{
			return v[vertex].neighbours[place];
		}

		Dart leg(LegIndex leg) const
		{
			return l[leg.index].leg;
		}

		LegIndex baseLeg() const
		{
			return base_leg;
		}

		LegIndex nextLegCCW(LegIndex leg) const
		{
			return l[leg.index].nextCCW;
		}

		LegIndex nextLegCW(LegIndex leg) const
		{
			return l[leg.index].nextCW;
		}

		LegIndex moveCCW(LegIndex leg, size_t times) const
		{
			for(size_t i = 0; i < times; i++)
				leg = nextLegCCW(leg);
			return leg;
		}

		LegIndex moveCW(LegIndex leg, size_t times) const
		{
			for(size_t i = 0; i < times; i++)
				leg = nextLegCW(leg);
			return leg;
		}

	public:
		IncrementalTangleBase();
		virtual ~IncrementalTangleBase();

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
		bool glue(size_t edgesToGlue, LegIndex legToGlue);
		void cancellGlue();
		Topology::EmbeddedGraph & toGraph() const;

	public:
		virtual size_t getPeriod() const = 0;
		virtual bool hasMirrorSymmetry() const = 0;
		virtual size_t getMirrorDifference() const = 0;

	protected:
		virtual bool preCheck(size_t legsToGlue, LegIndex posToGlue) = 0;
		virtual bool postCheck(size_t legsToGlue) = 0;

	private:
		void replaceLegsRange(LegIndex fromBegin, LegIndex fromEnd, LegIndex toBegin, LegIndex toEnd)
		{
			const LegIndex prev = l[fromBegin.index].nextCW;
			const LegIndex next = l[fromEnd.index].nextCCW;

			l[prev.index].nextCCW = toBegin;
			l[toBegin.index].nextCW = prev;
			l[toEnd.index].nextCCW = next;
			l[next.index].nextCW = toEnd;
		}
	};
}
