#pragma once

#include <cmath>

namespace Geometry
{
	class Angle
	{
	protected:
		double value;

	protected:
		Angle(double _value)
			: value(_value)
		{
		}

	public:
		double toRadians() const
		{
			return value;
		}

		double toDegrees() const
		{
			return 180.0 * value / pi;
		}

		double sin() const
		{
			return ::sin(value);
		}

		double cos() const
		{
			return ::cos(value);
		}

		double tan() const
		{
			return ::tan(value);
		}

		std::string toString() const;

	public:
		static Angle fromRadians(double value)
		{
			return Angle(value);
		}

		static Angle fromDegrees(double value)
		{
			return Angle(pi * value / 180.0);
		}

	public:
		static const double pi;
	};
}
