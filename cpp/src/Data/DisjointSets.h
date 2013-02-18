#pragma once

#include <cstddef>
#include <cassert>
#include "./UnCopyable.h"

namespace Data
{
	class DisjointSets : public UnCopyable
	{
	private:
		struct Node
		{
			Node * father;
			size_t rank;

			Node * findSet()
			{
				if(father != this)
					father = father->findSet();
				return father;
			}
		};

	private:
		Node * const data;
		const size_t _size;
		size_t number_of_sets;

	public:
		DisjointSets(size_t size)
			: data(new Node[size])
			, _size(size)
		{
			clear();
		}

		~DisjointSets()
		{
			delete[] data;
		}

		void clear()
		{
			for(size_t i = 0; i < _size; i++)
			{
				data[i].father = data + i;
				data[i].rank = 0;
			}
			number_of_sets = _size;
		}

		size_t size() const
		{
			return _size;
		}

		size_t numberOfSets() const
		{
			return number_of_sets;
		}

		size_t findSet(size_t index) const
		{
			assert(index < _size);
			return data[index].findSet() - data;
		}

		bool unionSet(size_t a, size_t b)
		{
			assert(a < _size);
			assert(b < _size);

			Node * an = data[a].findSet();
			Node * bn = data[b].findSet();

			if(an == bn)
				return false;

			if(an->rank >= bn->rank)
			{
				bn->father = an;
				if(an->rank == bn->rank)
					an->rank++;
			}
			else
				an->father = bn;

			number_of_sets--;
			return true;
		}
	};
}
