#include "./TangleSkeleton.h"

namespace Math { namespace KnotTh { namespace Tangles {

	bool Dart::cIsLeg() const
	{
		return isLeg();
	}

	bool Dart::cIsDart() const
	{
		return isDart();
	}


	void TangleSkeleton::release()
	{
		delete this;
	}

	size_t TangleSkeleton::cNumberOfCrossings() const
	{
		return numberOfCrossings();
	}

	size_t TangleSkeleton::cNumberOfLegs() const
	{
		return numberOfLegs();
	}

	Dart TangleSkeleton::cDart(size_t crossing, size_t place) const
	{
		return dart(crossing, place);
	}

	Dart TangleSkeleton::cLeg(size_t l) const
	{
		return leg(l);
	}

	Dart TangleSkeleton::cOpposite(Dart dart) const
	{
		return opposite(dart);
	}

	Dart TangleSkeleton::cNextCCW(Dart dart) const
	{
		return nextCCW(dart);
	}

	Dart TangleSkeleton::cNextCW(Dart dart) const
	{
		return nextCW(dart);
	}

	int TangleSkeleton::cLexicographicalCompare(Dart a, Dart b) const
	{
		if(a.isLeg())
		{
			if(!b.isLeg())
				return -1;

			if(a.place < b.place)
				return -1;

			if(a.place > b.place)
				return 1;

			return 0;
		}
		else
		{
			if(b.isLeg())
				return 1;

			if(a.crossing < b.crossing)
				return -1;

			if(a.crossing > b.crossing)
				return 1;

			if(a.place < b.place)
				return -1;

			if(a.place > b.place)
				return 1;

			return 0;
		}
	}

	Dart TangleSkeleton::cLexicographicallyNext(Dart dart) const
	{
		if(dart.isLeg())
		{
			if(dart.place == numberOfLegs() - 1)
				return Dart(1, 0);
			else
				return Dart(0, dart.place + 1);
		}
		else
		{
			if(dart.place == 3)
				return Dart(dart.crossing + 1, 0);
			else
				return Dart(dart.crossing, dart.place + 1);
		}
	}

	size_t TangleSkeleton::cLexicographicalRangeSize(Dart a, Dart b) const
	{
		if(a.isLeg())
		{
			if(b.isLeg())
			{
				if(a.place <= b.place)
					return 1 + b.place - a.place;
				else
					return 0;
			}
			else
				return (numberOfLegs() - a.place) + (b.crossing * 4 + b.place - 3);
		}
		else
		{
			if(b.isLeg())
				return 0;
			else
			{
				const size_t x = a.crossing * 4 + a.place;
				const size_t y = b.crossing * 4 + b.place;
				return (x <= y ? 1 + y - x : 0);
			}
		}
	}

}}}
