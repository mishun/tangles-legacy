#pragma once

#include <string>
#include "Vector2D.h"

namespace Geometry
{
	class Segment2D
	{
	public:
		Vector2D begin;
		Vector2D end;

	public:
		Segment2D(const Vector2D & b, const Vector2D & e)
			: begin(b)
			, end(e)
		{
		}

		Vector2D interpolate(double alpha) const
		{
			return begin * (1.0 - alpha) + end * alpha;
		}

		Vector2D interpolate(double alpha, double beta) const
		{
			return interpolate(alpha) + (end - begin).ort() * beta;
		}

		Vector2D middle() const
		{
			return interpolate(0.5);
		}

		std::string toString() const;
	};
}
