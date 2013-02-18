#pragma once

#include <cassert>
#include <cstring>
#include <sstream>

namespace ChordDiagrams
{
	class ChordDiagram
	{
	public:
		ChordDiagram(size_t _chords)
			: chords(_chords)
			, points(2 * _chords)
			, cur_chords(0)
			, list_head(0)
		{
			assert(2 * chords < MAX);
			for(size_t i = 0; i < points; i++)
			{
				a[i] = 0;
				list[i].next = i + 1;
				list[i].prev = (i == 0) ? points : i - 1;
			}
		}

		bool isFull() const
		{
			return cur_chords == chords;
		}

		size_t operator[](const size_t index) const
		{
			assert(index < points);
			return a[index];
		}

		bool isEmpty(const size_t index) const
		{
			assert(index < points);
			return a[index] == 0;
		}

		size_t firstEmpty() const
		{
			return list_head;
		}

		size_t nextEmpty(const size_t k) const
		{
			assert(k < points && isEmpty(k));
			return list[k].next;
		}

		void addChord(size_t u, size_t v)
		{
			assert(cur_chords < chords);
			assert(u < points && v < points && u != v);
			assert(isEmpty(u) && isEmpty(v));

			if(u > v)
				std::swap(u, v);

			a[u] = v - u;
			a[v] = points + u - v;

			removeFromList(u);
			removeFromList(v);

			history[cur_chords].u = u;
			history[cur_chords].v = v;
			cur_chords++;
		}

		void cancellChord()
		{
			assert(cur_chords > 0);

			cur_chords--;
			const size_t u = history[cur_chords].u;
			const size_t v = history[cur_chords].v;

			a[u] = 0;
			a[v] = 0;

			returnToList(v);
			returnToList(u);
		}

		size_t genus() const
		{
			assert(isFull());

			bool ok[MAX];
			memset(ok, 0, sizeof(bool[points]));

			size_t v = 0;
			for(size_t i = 0; i < points; i++)
				if(!ok[i])
				{
					v++;
					for(size_t j = i; !ok[j]; j = (j + a[j] + 1) % points)
						ok[j] = true;
				}

			return (chords + 1 - v) / 2;
		}

		std::string toString() const
		{
			std::ostringstream out;
			out << "[";
			for(size_t i = 0; i < points; i++)
			{
				if(i > 0)
					out << " ";

				if(isEmpty(i))
					out << "?";
				else
					out << (*this)[i];
			}
			out << "]";
			return out.str();
		}

	private:
		void removeFromList(const size_t x)
		{
			const size_t n = list[x].next;
			const size_t p = list[x].prev;

			if(list_head == x)
				list_head = n;

			if(n < points)
				list[n].prev = p;

			if(p < points)
				list[p].next = n;
		}

		void returnToList(const size_t x)
		{
			const size_t n = list[x].next;
			const size_t p = list[x].prev;

			if(list_head == n)
				list_head = x;

			if(n < points)
				list[n].prev = x;

			if(p < points)
				list[p].next = x;
		}

	private:
		struct ChordHistory
		{
			size_t u;
			size_t v;
		};

		struct ListElement
		{
			size_t next;
			size_t prev;
		};

	public:
		const size_t chords;
		const size_t points;

	public:
		static const size_t MAX = 64;

	private:
		size_t cur_chords;
		size_t list_head;
		size_t a[MAX];
		ListElement list[MAX];
		ChordHistory history[MAX];
	};
}
