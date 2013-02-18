#pragma once

#include <vector>
#include "Vector2D.h"
#include "LinearSystem.h"

namespace Geometry
{
	class RubberConfiguration
	{
	protected:
		const size_t size;
		LinearSystem x;
		LinearSystem y;

	public:
		RubberConfiguration(size_t);
		~RubberConfiguration();

		void clear();
		void connect(size_t, size_t, double);
		void connect(size_t, const Vector2D &, double);
		std::vector<Vector2D> solve();
	};
}
