#pragma once

#include <cstddef>
#include <Algebra/D4SubGroup.h>

namespace Tangles
{
	class ProjectionCrossing
	{
	public:
		size_t code() const
		{
			return 0;
		}

		const Algebra::D4SubGroup & symmetry() const
		{
			return Algebra::D4SubGroup::D4;
		}

	public:
		static const Algebra::D4SubGroup & globalSymmetry()
		{
			return Algebra::D4SubGroup::D4;
		}
	};
}
