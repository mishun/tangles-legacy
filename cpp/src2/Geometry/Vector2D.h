#pragma once

#include <cmath>
#include <string>

namespace Geometry
{
	class Vector2D
	{
	public:
		double x;
		double y;

	public:
		Vector2D(double _x = 0.0, double _y = 0.0)
			: x(_x)
			, y(_y)
		{
		}

		double abs() const
		{
			return sqrt(x * x + y * y);
		}

		double abs2() const
		{
			return x * x + y * y;
		}

		double atan2() const
		{
			return atan2l(y, x);
		}

		Vector2D normalized() const
		{
			return *this / abs();
		}

		Vector2D ort() const
		{
			return Vector2D(-y, x);
		}

		Vector2D operator+() const
		{
			return *this;
		}

		Vector2D operator-() const
		{
			return Vector2D(-x, -y);
		}

		Vector2D operator+(const Vector2D & that) const
		{
			return Vector2D(x + that.x, y + that.y);
		}

		Vector2D operator-(const Vector2D & that) const
		{
			return Vector2D(x - that.x, y - that.y);
		}

		Vector2D operator*(double mul) const
		{
			return Vector2D(x * mul, y * mul);
		}

		Vector2D operator/(double div) const
		{
			return *this * (1.0 / div);
		}

		double operator*(const Vector2D & that) const
		{
			return x * that.x + y * that.y;
		}

		double operator^(const Vector2D & that) const
		{
			return x * that.y - y * that.x;
		}

		Vector2D & operator+=(const Vector2D & that)
		{
			x += that.x, y += that.y;
			return *this;
		}

		Vector2D & operator-=(const Vector2D & that)
		{
			x -= that.x, y -= that.y;
			return *this;
		}

		std::string toString() const;
		double distToSegment(Vector2D, Vector2D) const;

	public:
		static Vector2D polar(double phi)
		{
			return Vector2D(cos(phi), sin(phi));
		}

		static Vector2D polar(double r, double phi)
		{
			return Vector2D(r * cos(phi), r * sin(phi));
		}

		static Vector2D getCircleCenter(Vector2D, Vector2D, Vector2D);
		static Vector2D intersection(Vector2D, Vector2D, Vector2D, Vector2D);
	};
}
