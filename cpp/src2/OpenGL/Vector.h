#pragma once

#include <cmath>
#include <string>

namespace OpenGL
{
	template<const size_t dim, class Element>
	class Vector
	{
	public:
		Element data[dim];

	public:
		inline const Vector operator+(const Vector & that) const
		{
			Vector res;
			add(*this, that, res);
			return res;
		}

	public:
		static inline void add(const Vector & a, const Vector & b, Vector & res)
		{
			for(size_t i = 0; i < dim; i++)
				res.data[i] = a.data[i] + b.data[i];
		}

		static inline void subtract(const Vector & a, const Vector & b, Vector & res)
		{
			for(size_t i = 0; i < dim; i++)
				res.data[i] = a.data[i] - b.data[i];
		}

		static inline Element dot(const Vector & a, const Vector & b)
		{
			Element dot = 0;
			for(size_t i = 0; i < dim; i++)
				dot += a.data[i] * b.data[i];
			return dot;
		}
	};

	template<class Element>
	class Vector<2, Element>
	{
	public:
		Element x;
		Element y;

	public:
		inline Vector(Element _x = 0, Element _y = 0) : x(_x), y(_y) {}

		inline const Element * data() const
		{
			return &x;
		}

		inline const Vector operator+() const { return *this; }
		inline const Vector operator-() const { return Vector(-x, -y); }

		inline const Vector operator+(const Vector & v) const { return Vector(x + v.x, y + v.y); }
		inline const Vector operator-(const Vector & v) const { return Vector(x - v.x, y - v.y); }

		inline const Vector & operator+=(const Vector & v) { x += v.x, y += v.y; return *this; }
		inline const Vector & operator-=(const Vector & v) { x -= v.x, y -= v.y; return *this; }

		inline const Vector operator*(Element m) const { return Vector(x * m, y * m); }
		inline const Vector & operator*=(Element m) { x *= m, y *= m; return *this; }

		inline Element operator*(const Vector & v) const { return x * v.x + y * v.y; }
		inline Element operator^(const Vector & v) const { return x * v.y - y * v.x; }

		inline Element squaredLength() const { return            x * x + y * y;  }
		inline Element length() const        { return       sqrt(x * x + y * y); }
		inline Element inverseLength() const { return 1.0 / sqrt(x * x + y * y); }

		inline void normalize() { *this *= inverseLength(); }
		inline const Vector normalized() const { return *this * inverseLength(); }

		const std::string toString() const;
	};

	template<class Element>
	class Vector<3, Element>
	{
	public:
		Element x;
		Element y;
		Element z;

	public:
		inline Vector(Element _x = 0, Element _y = 0, Element _z = 0)
			: x(_x), y(_y), z(_z)
		{
		}

		inline const Element * data() const
		{
			return &x;
		}

		const Vector operator+() const { return *this; }
		const Vector operator-() const { return Vector(-x, -y, -z); }

		const Vector operator+(const Vector & v) const { return Vector(x + v.x, y + v.y, z + v.z); }
		const Vector operator-(const Vector & v) const { return Vector(x - v.x, y - v.y, z - v.z); }

		const Vector & operator+=(const Vector & v) { x += v.x, y += v.y, z += v.z; return *this; }
		const Vector & operator-=(const Vector & v) { x -= v.x, y -= v.y, z -= v.z; return *this; }

		const Vector operator*(Element m) const { return Vector(x * m, y * m, z * m); }
		const Vector & operator*=(Element m) { x *= m, y *= m, z *= m; return *this; }

		inline Element operator*(const Vector & v) const { return x * v.x + y * v.y + z * v.z; }

		inline const Vector operator^(const Vector & v) const
		{
			return Vector(
				y * v.z - z * v.y,
				z * v.x - x * v.z,
				x * v.y - y * v.x);
		}

		Element squaredLength() const { return            x * x + y * y + z * z;  }
		Element length() const        { return       sqrt(x * x + y * y + z * z); }
		Element inverseLength() const { return 1.0 / sqrt(x * x + y * y + z * z); }

		const Vector normalized() const { return *this * inverseLength(); }
		void normalize() { *this *= inverseLength(); }

		const std::string toString() const;
	};

	template<class Element>
	class Vector<4, Element>
	{
	public:
		Element x;
		Element y;
		Element z;
		Element w;

	public:
		inline Vector(Element _x = 0, Element _y = 0, Element _z = 0, Element _w = 0)
			: x(_x), y(_y), z(_z), w(_w)
		{
		}

		inline Vector(const Vector<3, Element> & v, Element _w)
			: x(v.x), y(v.y), z(v.z), w(_w)
		{
		}

		inline const Element * data() const
		{
			return &x;
		}

		inline const Vector<3, Element> projection() const
		{
			return Vector<3, Element>(x / w, y / w, z / w);
		}

		const std::string toString() const;
	};


	typedef Vector<2, float>  Vector2f;
	typedef Vector<2, double> Vector2d;
	typedef Vector<3, float>  Vector3f;
	typedef Vector<3, double> Vector3d;
	typedef Vector<4, float>  Vector4f;
	typedef Vector<4, double> Vector4d;
}
