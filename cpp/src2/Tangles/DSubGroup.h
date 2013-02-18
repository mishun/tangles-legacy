#pragma once

#include <cstddef>

namespace Tangles
{
	class DSubGroup
	{
	private:
	public:
		size_t numberOfPoints() const;
		size_t rotationPeriod() const;
	};
}
