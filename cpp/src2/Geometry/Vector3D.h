#pragma once

#include <cmath>
#include <string>

namespace Geometry
{
	class Vector3D
	{
	public:
		double x;
		double y;
		double z;

	public:
		Vector3D(double _x = 0.0, double _y = 0.0, double _z = 0.0)
			: x(_x)
			, y(_y)
			, z(_z)
		{
		}

		double abs() const
		{
			return sqrt(x * x + y * y + z * z);
		}

		double abs2() const
		{
			return x * x + y * y + z * z;
		}

		Vector3D normalized() const
		{
			return *this / abs();
		}

		Vector3D operator+() const
		{
			return *this;
		}

		Vector3D operator-() const
		{
			return Vector3D(-x, -y, -z);
		}

		Vector3D operator+(const Vector3D & that) const
		{
			return Vector3D(x + that.x, y + that.y, z + that.z);
		}

		Vector3D operator-(const Vector3D & that) const
		{
			return Vector3D(x - that.x, y - that.y, z - that.z);
		}

		Vector3D operator*(double mul) const
		{
			return Vector3D(x * mul, y * mul, z * mul);
		}

		Vector3D operator/(double div) const
		{
			return *this * (1.0 / div);
		}

		double operator*(const Vector3D & that) const
		{
			return x * that.x + y * that.y + z * that.z;
		}

		const Vector3D operator^(const Vector3D & that) const
		{
			return Vector3D(
				y * that.z - z * that.y,
				z * that.x - x * that.z,
				x * that.y - y * that.x);
		}

		Vector3D & operator+=(const Vector3D & that)
		{
			x += that.x;
			y += that.y;
			z += that.z;
			return *this;
		}

		Vector3D & operator-=(const Vector3D & that)
		{
			x -= that.x;
			y -= that.y;
			z -= that.z;
			return *this;
		}

		std::string toString() const;
	};
}
