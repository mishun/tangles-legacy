#pragma once

#include <cassert>
#include <Util/Util.h>
#include "SubTangle.h"

namespace Tangles
{
	class TangleDataStructure : public Util::IDestroyable
	{
	protected:
		static const size_t max_log_size = 5;
		static const size_t max_subtangles = (1 << max_log_size) - 1;

	private:
		struct Vertex
		{
			friend class TangleDataStructure;

		public:
			size_t crossings() const
			{
				return subtangle->crossings();
			}

			size_t subtangleCode() const
			{
				return subtangle->code();
			}

			D4Group getOrientation() const
			{
				return orientation;
			}

			const SubTangle & getSubTangle() const
			{
				return *subtangle;
			}

			size_t orientationCode(D4Group g) const
			{
				return subtangle->symmetry.factorSet(g * orientation);
			}

			bool axis(D4Group g) const
			{
				return subtangle->axis(g * orientation);
			}

			TangleGraph getSubGraph() const
			{
				return subtangle->getGraph().transform(orientation.rotation(), orientation.mirror());
			}

		private:
			size_t neighbours[4];
			D4Group orientation;
			const SubTangle * subtangle;
		};

	protected:
		TangleDataStructure(const SubTangle & sub)
			: _crossings(sub.crossings())
			, _subtangles(1)
			, _legs(4)
			, memory(new char[_subtangles * sizeof(Vertex) + _legs * sizeof(size_t)])
			, st(reinterpret_cast<Vertex *>(memory) - 1)
			, l(reinterpret_cast<size_t *>(st + _subtangles + 1))
		{
			st[1].subtangle = &sub;
			st[1].orientation = D4Group::identity;

			st[1].neighbours[0] = 0;
			st[1].neighbours[1] = 0;
			st[1].neighbours[2] = 0;
			st[1].neighbours[3] = 0;

			l[0] = 4 * 1 + 0;
			l[1] = 4 * 1 + 1;
			l[2] = 4 * 1 + 2;
			l[3] = 4 * 1 + 3;
		}

		//       glue_edges = 1                  glue_edges = 2                  glue_edges = 3
		// ........|                       ........|                       ........|
		// (pos+1)-|---------------3       (pos+1)-|---------------2       (pos+1)-|---------------1
		//         |  +=========+                  |  +=========+                  |  +=========+
		//  (pos)--|--|-0-\ /-3-|--2        (pos)--|--|-0-\ /-3-|--1        (pos)--|--|-0-\ /-3-|--0
		// ........|  |    *    |                  |  |    *    |                  |  |    *    |
		// ........|  |   / \-2-|--1       (pos-1)-|--|-1-/ \-2-|--0       (pos-1)-|--|-1-/ \   |
		// ........|  |  1      |          ........|  +=========+                  |  |      2  |
		// ........|  |   \-----|--0       ........|                       (pos-2)-|--|-----/   |
		// ........|  +=========+          ........|                       ........|  +=========+
		TangleDataStructure(const TangleDataStructure & father, const SubTangle & sub, D4Group orient, const size_t pos, const size_t glue_edges)
			: _crossings(father.crossings() + sub.crossings())
			, _subtangles(father.subtangles() + 1)
			, _legs(father.legs() + 4 - 2 * glue_edges)
			, memory(new char[_subtangles * sizeof(Vertex) + _legs * sizeof(size_t)])
			, st(reinterpret_cast<Vertex *>(memory) - 1)
			, l(reinterpret_cast<size_t *>(st + _subtangles + 1))
		{
			assert(father.subtangles() < max_subtangles);
			assert(pos < father.legs());
			assert(glue_edges >= 1 && glue_edges <= 3 && glue_edges <= father.legs());

			for(size_t i = 1; i <= father.subtangles(); i++)
				st[i] = father.st[i];

			Vertex & cur = st[last()];
			cur.subtangle = &sub;
			cur.orientation = orient;

			for(size_t i = 0, j = pos; i < glue_edges; i++, j = (j + father.legs() - 1) % father.legs())
			{
				size_t tmp = father.l[j];
				cur.neighbours[i] = tmp;
				st[tmp >> 2].neighbours[tmp & 3] = 4 * last() + i;
			}

			{
				size_t j = 0;
				for(size_t i = glue_edges; i < 4; i++)
				{
					cur.neighbours[i] = 0;
					l[j++] = 4 * last() + i;
				}

				const size_t up = (pos + father.legs() - glue_edges + 1) % father.legs();
				for(size_t i = (pos + 1) % father.legs(); i != up; i = (i + 1) % father.legs())
					l[j++] = father.l[i];
			}
		}

		virtual ~TangleDataStructure()
		{
			delete[] memory;
		}

	public:
		virtual void destroy()
		{
			delete this;
		}

		size_t crossings() const
		{
			return _crossings;
		}

		size_t subtangles() const
		{
			return _subtangles;
		}

		size_t legs() const
		{
			return _legs;
		}

		size_t last() const
		{
			return _subtangles;
		}

		const Vertex & vertex(size_t stangle) const
		{
			assert(1 <= stangle && stangle <= subtangles());
			return st[stangle];
		}

		//            |3   2|
		//            | \ / |
		// [n << 2] + |  n  |
		//            | / \ |
		//            |0   1|
		size_t neighbour(size_t stangle, size_t index) const
		{
			assert(1 <= stangle && stangle <= subtangles());
			assert(index < 4);
			return st[stangle].neighbours[index];
		}

		//   <------
		//    | | |
		//   +-----+
		// --|     |-- 2
		// --|     |-- 1
		// --|     |-- 0
		//   +-----+
		//    | | | 
		//   ---> (legs-1)
		size_t border(size_t leg) const
		{
			assert(leg < legs());
			return l[leg];
		}

	private:
		const size_t _crossings;
		const size_t _subtangles;
		const size_t _legs;
		char * memory;
		Vertex * st;
		size_t * l;
	};
}
