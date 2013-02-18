#pragma once

#include <cstddef>
#include <Algebra/D4SubGroup.h>

namespace Tangles
{
	class ArbitraryCrossing
	{
	public:
		size_t code() const
		{
			return 0;
		}

		const Algebra::D4SubGroup & symmetry() const
		{
			return Algebra::D4SubGroup::GS;
		}

		bool passOver(size_t i) const
		{
			return (i & 1) == 0;
		}

		bool passUnder(size_t i) const
		{
			return (i & 1) == 1;
		}

	public:
		static const Algebra::D4SubGroup & globalSymmetry()
		{
			return Algebra::D4SubGroup::GS;
		}

		template<template<typename> class Tangle>
		static bool passOver(const Tangle<ArbitraryCrossing> & tangle, const Dart u)
		{
			const size_t rel = tangle.rotation(u.crossing).inverse().map(u.place);
			return tangle.crossing(u.crossing).passOver(rel);
		}

		template<template<typename> class Tangle>
		static bool passUnder(const Tangle<ArbitraryCrossing> & tangle, const Dart u)
		{
			const size_t rel = tangle.rotation(u.crossing).inverse().map(u.place);
			return tangle.crossing(u.crossing).passUnder(rel);
		}
	};
}
