#pragma once

#include <cassert>
#include <cstddef>
#include <Algebra/D4Group.h>
#include <Data/SharedPointer.h>
#include "./Dart.h"
#include "./TangleUtil.h"

namespace Tangles
{
	template<typename Crossing>
	class TangleStorage
	{
	private:
		struct VertexData
		{
			Dart neighbours[4];
			Algebra::D4Group rotation;
			Crossing crossing;
		};

	public:
		typedef Crossing CrossingType;

		class LegIterator
		{
			friend class TangleStorage;

		private:
			const TangleStorage * owner;
			size_t index;

		private:
			LegIterator(const TangleStorage * o, size_t i)
				: owner(o)
				, index(i)
			{
			}

		public:
			LegIterator(const LegIterator & that)
				: owner(that.owner)
				, index(that.index)
			{
			}

			const LegIterator & operator=(const LegIterator & that)
			{
				owner = that.owner;
				index = that.index;
				return *this;
			}

			LegIterator nextCCW() const
			{
				return LegIterator(owner, (index + 1) % owner->numberOfLegs());
			}

			LegIterator nextCW() const
			{
				return LegIterator(owner, (index + owner->numberOfLegs() - 1) % owner->numberOfLegs());
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
				return owner->leg(index);
			}

			bool operator==(const LegIterator & that) const
			{
				return owner == that.owner && index == that.index;
			}

			bool operator!=(const LegIterator & that) const
			{
				return owner != that.owner || index != that.index;
			}
		};

	private:
		const size_t crossings;
		const size_t legs;
		VertexData * const v;
		Dart * const l;

	public:
		template<class T>
		explicit TangleStorage(const T & that)
			: crossings(that.numberOfCrossings())
			, legs(that.numberOfLegs())
			, v(new VertexData[that.numberOfCrossings() + 1])
			, l(new Dart[that.numberOfLegs()])
		{
			for(size_t i = 1; i <= that.numberOfCrossings(); i++)
			{
				v[i].crossing = that.crossing(i);
				v[i].rotation = that.rotation(i);
				for(size_t j = 0; j < 4; j++)
					v[i].neighbours[j] = that.neighbour(Dart(j, i));
			}

			auto it = that.baseLeg();
			for(size_t i = 0; i < that.numberOfLegs(); i++, it = it.nextCCW())
				l[i] = it.leg();
		}

		template<class NeighboursList, class CrossingsList, class RotationsList, class LegsList>
		TangleStorage(const size_t _cn, const size_t _ln, const NeighboursList & _n, const CrossingsList & _c, const RotationsList & _r, const LegsList & _l)
			: crossings(_cn)
			, legs(_ln)
			, v(new VertexData[_cn + 1])
			, l(new Dart[_ln])
		{
			assert(_cn > 0);
			assert(_ln >= 4 && _ln % 2 == 0);

			for(size_t i = 1; i <= _cn; i++)
			{
				v[i].crossing = _c[i];
				v[i].rotation = _r[i];
				for(size_t j = 0; j < 4; j++)
					v[i].neighbours[j] = _n[i] [j];
			}

			for(size_t i = 0; i < _ln; i++)
				l[i] = _l[i];

			{
				for(size_t i = 1; i <= numberOfCrossings(); i++)
					for(size_t j = 0; j < 4; j++)
					{
						const Dart d(j, i);
						const Dart e = neighbour(d);
						assert(e.onBorder() || neighbour(e) == d);
					}

				auto it = baseLeg();
				for(size_t i = 0; i < numberOfLegs(); i++, it = it.nextCCW())
				{
					const Dart leg = it.leg();
					assert(neighbour(leg).onBorder());
				}
			}
		}

		~TangleStorage()
		{
			delete[] v;
		}

		size_t numberOfCrossings() const
		{
			return crossings;
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
			assert(dart.crossing > 0);
			assert(dart.crossing <= numberOfCrossings());
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

		Dart leg(size_t index) const
		{
			assert(index < numberOfLegs());
			return l[index];
		}

		LegIterator baseLeg() const
		{
			return LegIterator(this, 0);
		}

		bool operator==(const TangleStorage & that) const
		{
			return diskHomeomorphismInvariant(*this) == diskHomeomorphismInvariant(that);
		}

		bool operator!=(const TangleStorage & that) const
		{
			return !operator==(that);
		}

		bool operator<(const TangleStorage & that) const
		{
			return diskHomeomorphismInvariant(*this) < diskHomeomorphismInvariant(that);
		}
	};

	template<template<typename C> class Tangle, typename C>
	inline Data::SharedPointer< TangleStorage<C> > toStorage(const Tangle<C> & tangle)
	{
		return Data::SharedPointer< TangleStorage<C> >(new TangleStorage<C>(tangle));
	}
}
