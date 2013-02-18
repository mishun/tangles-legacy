#pragma once

#include <algorithm>
#include "./PersistentArrayBT.h"

namespace Data { namespace Native {

	class DisjointSet
	{
	private:
		size_t referenceCounter;
		const size_t size;
		Native::PersistentArrayBT<size_t> father;
		Native::PersistentArrayBT<size_t> rank;

	private:
		DisjointSet(const size_t sz, const Native::PersistentArrayBT<size_t> & f, const Native::PersistentArrayBT<size_t> & r)
			: referenceCounter(1)
			, size(sz)
			, father(f)
			, rank(r)
		{
		}

		~DisjointSet()
		{
		}

		size_t findSet(const size_t x)
		{
			const size_t px = father[x];
			if(px == x)
				return x;

			const size_t ppx = father[px];
			if(px == ppx)
				return px;

			const size_t f = findSet(ppx);

			father = father.set(x, f);

			if(f != ppx)
				father = father.set(px, f);

			return f;
		}

		DisjointSet * unionSet(size_t x, size_t y)
		{
			if(x == y || (x = findSet(x)) == (y = findSet(y)))
			{
				referenceCounter++;
				return this;
			}

			size_t rx = rank[x];
			size_t ry = rank[y];

			if(rx < ry)
				std::swap(x, y);

			return new DisjointSet(size, father.set(y, x), (rx == ry) ? rank.set(x, rx + 1) : rank);
		}

	public:
		static DisjointSet * create(size_t);
		static size_t findSet(size_t, DisjointSet *);
		static DisjointSet * unionSet(size_t, size_t, DisjointSet *);
		static void release(DisjointSet *);
		static DisjointSet * growToSize(size_t, DisjointSet *);
	};

}}
