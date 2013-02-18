#pragma once

#include <cstddef>
#include <utility>

namespace Tangles
{
	struct Dart
	{
	public:
		size_t place : 2;
		size_t crossing : 6;

	public:
		Dart(size_t p = 0, size_t c = 0)
			: place(p)
			, crossing(c)
		{
		}

		Dart nextCCW() const
		{
			//return Dart { (place + 1) & 3, crossing };
			return Dart((place + 1) & 3, crossing);
		}

		Dart nextCW() const
		{
			//return Dart { (place - 1) & 3, crossing };
			return Dart((place - 1) & 3, crossing);
		}

		Dart next(int dir) const
		{
			//return Dart { (place + dir) & 3, crossing };
			return Dart((place + dir) & 3, crossing);
		}

		Dart opposite() const
		{
			//return Dart { place ^ 2, crossing };
			return Dart(place ^ 2, crossing);
		}

		bool onBorder() const
		{
			return crossing == 0;
		}

		size_t offset() const
		{
			return (crossing << 2) + place;
		}

		std::pair<size_t, size_t> toPair() const
		{
			assert(crossing != 0);
			return std::make_pair(static_cast<size_t>(place), static_cast<size_t>(crossing));
		}

		bool operator==(Dart that) const
		{
			return place == that.place && crossing == that.crossing;
		}

		bool operator!=(Dart that) const
		{
			return place != that.place || crossing != that.crossing;
		}
	};
}
