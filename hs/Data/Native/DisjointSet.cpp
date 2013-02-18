#include "./DisjointSet.h"

namespace Data { namespace Native {

	DisjointSet * DisjointSet::create(const size_t size)
	{
		size_t * f = new size_t[size];
		size_t * r = new size_t[size];

		for(size_t i = 0; i < size; i++)
			f[i] = i, r[i] = 0;

		return new DisjointSet(size, PersistentArrayBT<size_t>(f), PersistentArrayBT<size_t>(r));
	}

	size_t DisjointSet::findSet(const size_t x, DisjointSet * set)
	{
		return set->findSet(x);
	}

	DisjointSet * DisjointSet::unionSet(const size_t a, const size_t b, DisjointSet * set)
	{
		return set->unionSet(a, b);
	}

	void DisjointSet::release(DisjointSet * set)
	{
		if(--(set->referenceCounter) == 0)
			delete set;
	}

	DisjointSet * DisjointSet::growToSize(const size_t size, DisjointSet * set)
	{
		size_t * f = new size_t[size];
		size_t * r = new size_t[size];

		for(size_t i = 0; i < set->size; i++)
			f[i] = set->findSet(i);

		for(size_t i = set->size; i < size; i++)
			f[i] = i;

		for(size_t i = 0; i < size; i++)
			r[i] = 0;

		return new DisjointSet(size, PersistentArrayBT<size_t>(f), PersistentArrayBT<size_t>(r));
	}

}}
