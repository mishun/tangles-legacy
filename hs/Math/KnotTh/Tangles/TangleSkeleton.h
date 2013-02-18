#pragma once

#include <cstddef>
#include <cassert>

namespace Math { namespace KnotTh { namespace Tangles {

	struct Dart
	{
		friend class BITangle;
	//	friend class TangleSkeleton;

	//private:
		Dart(size_t c, size_t p)
			: place(p)
			, crossing(c)
		{
		}

	public:
		size_t place : 8;
		size_t crossing : (8 * sizeof(size_t) - 8);

	public:
		bool isLeg() const
		{
			return crossing == 0;
		}

		bool isDart() const
		{
			return crossing != 0;
		}

	public:
		bool cIsLeg() const;
		bool cIsDart() const;
	};

	class TangleSkeleton
	{
	protected:
		struct CData
		{
			Dart neighbours[4];
		};

	protected:
		const size_t _cn;
		const size_t _ln;
		char * const mem;
		CData * const _c;
		Dart * const _l;

	protected:
		TangleSkeleton(const size_t cn, const size_t ln)
			: _cn(cn)
			, _ln(ln)
			, mem(new char[sizeof(CData) * (cn + 1) + sizeof(Dart) * ln])
			, _c(reinterpret_cast<CData *>(mem + sizeof(Dart) * ln))
			, _l(reinterpret_cast<Dart *>(mem))
		{
		}

		virtual ~TangleSkeleton()
		{
			delete[] mem;
		}

	public:
		size_t numberOfCrossings() const
		{
			return _cn;
		}

		size_t numberOfLegs() const
		{
			return _ln;
		}

		Dart dart(size_t crossing, size_t place) const
		{
			assert(crossing > 0 && crossing <= _cn);
			return Dart(crossing, place & 3);
		}

		Dart leg(size_t l) const
		{
			return Dart(0, l % _ln);
		}

		Dart opposite(Dart dart) const
		{
			if(dart.isLeg())
			{
				assert(dart.place < _ln);
				return _l[dart.place];
			}
			else
			{
				assert(dart.crossing <= _cn);
				assert(dart.place < 4);
				return _c[dart.crossing].neighbours[dart.place];
			}
		}

		Dart nextCCW(Dart dart) const
		{
			if(dart.isLeg())
				return Dart(0, (dart.place + 1) % numberOfLegs());
			else
				return Dart(dart.crossing, (dart.place + 1) & 3);
		}

		Dart nextCW(Dart dart) const
		{
			if(dart.isLeg())
				return Dart(0, (dart.place + numberOfLegs() - 1) % numberOfLegs());
			else
				return Dart(dart.crossing, (dart.place - 1) & 3);
		}

	public:
		void release();

	public:
		size_t cNumberOfCrossings() const;
		size_t cNumberOfLegs() const;
		Dart cDart(size_t, size_t) const;
		Dart cLeg(size_t) const;
		Dart cOpposite(Dart) const;
		Dart cNextCCW(Dart) const;
		Dart cNextCW(Dart) const;

		int cLexicographicalCompare(Dart, Dart) const;
		Dart cLexicographicallyNext(Dart) const;
		size_t cLexicographicalRangeSize(Dart, Dart) const;
	};

}}}
