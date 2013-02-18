#pragma once

#include <string>
#include "Vector.h"

namespace Kino
{
	template<const size_t dim, class Element>
	class Matrix
	{
	public:
		Element data[dim] [dim];

	public:
		inline const Matrix operator+(const Matrix & that) const
		{
			Matrix res;
			add(*this, that, res);
			return res;
		}

		inline const Matrix operator-(const Matrix & that) const
		{
			Matrix res;
			subtract(*this, that, res);
			return res;
		}

		inline const Matrix operator*(const Matrix & that) const
		{
			Matrix res;
			multiply(*this, that, res);
			return res;
		}

	public:
		inline static void add(const Matrix & a, const Matrix & b, Matrix & res)
		{
			for(size_t i = 0; i < dim * dim; i++)
				for(size_t j = 0; j < dim; j++)
					res.data[i] [j] = a.data[i] [j] + b.data[i] [j];
		}

		inline static void subtract(const Matrix & a, const Matrix & b, Matrix & res)
		{
			for(size_t i = 0; i < dim * dim; i++)
				for(size_t j = 0; j < dim; j++)
					res.data[i] [j] = a.data[i] [j] - b.data[i] [j];
		}

		inline static void multiply(const Matrix & a, const Matrix & b, Matrix & res)
		{
			for(size_t i = 0; i < dim; i++)
				for(size_t j = 0; j < dim; j++)
				{
					Element cur = 0;
					for(size_t k = 0; k < dim; k++)
						cur += a.data[k] [i] * b.data[j] [k];
					res.data[j] [i] = cur;
				}
		}

		inline static void multiply(const Matrix & a, const Vector<dim, Element> & b, Vector<dim, Element> & res)
		{
			for(size_t i = 0; i < dim; i++)
			{
				Element cur = 0;
				for(size_t j = 0; j < dim; j++)
					cur += a.data[j] [i] * b.data[j];
				res.data[i] = cur;
			}
		}
	};

	template<class Element>
	class Matrix<4, Element>
	{
	public:
		Element xx, yx, zx, wx;
		Element xy, yy, zy, wy;
		Element xz, yz, zz, wz;
		Element xw, yw, zw, ww;

	public:
		inline Matrix()
			: xx(0), yx(0), zx(0), wx(0)
			, xy(0), yy(0), zy(0), wy(0)
			, xz(0), yz(0), zz(0), wz(0)
			, xw(0), yw(0), zw(0), ww(0)
		{
		}

		inline const Element * data() const
		{
			return &xx;
		}

		inline const Matrix operator+(const Matrix & m) const
		{
			Matrix res;
			res.xx = xx + m.xx; res.yx = yx + m.yx; res.zx = zx + m.zx; res.wx = wx + m.wx;
			res.xy = xy + m.xy; res.yy = yy + m.yy; res.zy = zy + m.zy; res.wy = wy + m.wy;
			res.xz = xz + m.xz; res.yz = yz + m.yz; res.zz = zz + m.zz; res.wz = wz + m.wz;
			res.xw = xw + m.xw; res.yw = yw + m.yw; res.zw = zw + m.zw; res.ww = ww + m.ww;
			return res;
		}

		inline const Matrix operator-(const Matrix & m) const
		{
			Matrix res;
			res.xx = xx - m.xx; res.yx = yx - m.yx; res.zx = zx - m.zx; res.wx = wx - m.wx;
			res.xy = xy - m.xy; res.yy = yy - m.yy; res.zy = zy - m.zy; res.wy = wy - m.wy;
			res.xz = xz - m.xz; res.yz = yz - m.yz; res.zz = zz - m.zz; res.wz = wz - m.wz;
			res.xw = xw - m.xw; res.yw = yw - m.yw; res.zw = zw - m.zw; res.ww = ww - m.ww;
			return res;
		}

		inline const Matrix operator*(const Matrix & m) const
		{
			Matrix res;
			res.xx = xx * m.xx + xy * m.yx + xz * m.zx + xw * m.wx;
			res.yx = yx * m.xx + yy * m.yx + yz * m.zx + yw * m.wx;
			res.zx = zx * m.xx + zy * m.yx + zz * m.zx + zw * m.wx;
			res.wx = wx * m.xx + wy * m.yx + wz * m.zx + ww * m.wx;
			res.xy = xx * m.xy + xy * m.yy + xz * m.zy + xw * m.wy;
			res.yy = yx * m.xy + yy * m.yy + yz * m.zy + yw * m.wy;
			res.zy = zx * m.xy + zy * m.yy + zz * m.zy + zw * m.wy;
			res.wy = wx * m.xy + wy * m.yy + wz * m.zy + ww * m.wy;
			res.xz = xx * m.xz + xy * m.yz + xz * m.zz + xw * m.wz;
			res.yz = yx * m.xz + yy * m.yz + yz * m.zz + yw * m.wz;
			res.zz = zx * m.xz + zy * m.yz + zz * m.zz + zw * m.wz;
			res.wz = wx * m.xz + wy * m.yz + wz * m.zz + ww * m.wz;
			res.xw = xx * m.xw + xy * m.yw + xz * m.zw + xw * m.ww;
			res.yw = yx * m.xw + yy * m.yw + yz * m.zw + yw * m.ww;
			res.zw = zx * m.xw + zy * m.yw + zz * m.zw + zw * m.ww;
			res.ww = wx * m.xw + wy * m.yw + wz * m.zw + ww * m.ww;
			return res;
		}

		inline const Vector<4, Element> operator*(const Vector<4, Element> & v) const
		{
			return Vector<4, Element>(
				xx * v.x + xy * v.y + xz * v.z + xw * v.w,
				yx * v.x + yy * v.y + yz * v.z + yw * v.w,
				zx * v.x + zy * v.y + zz * v.z + zw * v.w,
				wx * v.x + wy * v.y + wz * v.z + ww * v.w);
		}

		Element determinant() const;
		const Matrix inverse() const;
		
		inline const Vector<4, Element> rowX() const { return Vector<4, Element>(xx, xy, xz, xw); }
		inline const Vector<4, Element> rowY() const { return Vector<4, Element>(yx, yy, yz, yw); }
		inline const Vector<4, Element> rowZ() const { return Vector<4, Element>(zx, zy, zz, zw); }
		inline const Vector<4, Element> rowW() const { return Vector<4, Element>(wx, wy, wz, ww); }

		inline const Vector<4, Element> colX() const { return Vector<4, Element>(xx, yx, zx, wx); }
		inline const Vector<4, Element> colY() const { return Vector<4, Element>(xy, yy, zy, wy); }
		inline const Vector<4, Element> colZ() const { return Vector<4, Element>(xz, yz, zz, wz); }
		inline const Vector<4, Element> colW() const { return Vector<4, Element>(xw, yw, zw, ww); }

		const std::string toString() const;
	};

	template<class Element>
	class Matrix<3, Element>
	{
	public:
		Element xx, yx, zx;
		Element xy, yy, zy;
		Element xz, yz, zz;

	public:
		inline Matrix()
			: xx(0), yx(0), zx(0)
			, xy(0), yy(0), zy(0)
			, xz(0), yz(0), zz(0)
		{
		}

		inline const Matrix operator+(const Matrix & m) const
		{
			Matrix res;
			res.xx = xx + m.xx; res.yx = yx + m.yx; res.zx = zx + m.zx;
			res.xy = xy + m.xy; res.yy = yy + m.yy; res.zy = zy + m.zy;
			res.xz = xz + m.xz; res.yz = yz + m.yz; res.zz = zz + m.zz;
			return res;
		}

		inline const Matrix operator-(const Matrix & m) const
		{
			Matrix res;
			res.xx = xx - m.xx; res.yx = yx - m.yx; res.zx = zx - m.zx;
			res.xy = xy - m.xy; res.yy = yy - m.yy; res.zy = zy - m.zy;
			res.xz = xz - m.xz; res.yz = yz - m.yz; res.zz = zz - m.zz;
			return res;
		}

		inline const Matrix operator*(const Matrix & m) const
		{
			Matrix res;
			res.xx = xx * m.xx + xy * m.yx + xz * m.zx;
			res.xy = xx * m.xy + xy * m.yy + xz * m.zy;
			res.xz = xx * m.xz + xy * m.yz + xz * m.zz;
			res.yx = yx * m.xx + yy * m.yx + yz * m.zx;
			res.yy = yx * m.xy + yy * m.yy + yz * m.zy;
			res.yz = yx * m.xz + yy * m.yz + yz * m.zz;
			res.zx = zx * m.xx + zy * m.yx + zz * m.zx;
			res.zy = zx * m.xy + zy * m.yy + zz * m.zy;
			res.zz = zx * m.xz + zy * m.yz + zz * m.zz;
			return res;
		}

		inline const Vector<3, Element> operator*(const Vector<3, Element> & v) const
		{
			return Vector<3, Element>(
				xx * v.x + xy * v.y + xz * v.z,
				yx * v.x + yy * v.y + yz * v.z,
				zx * v.x + zy * v.y + zz * v.z);
		}

		inline Element determinant() const
		{
			return xx * yy * zz + xy * yz * zx + xz * yx * zy - xx * yz * zy - xy * yx * zz - xz * yy * zx;
		}

		inline const Matrix inverse() const
		{
			Element det = determinant();
			Matrix res;
			res.xx = (yy * zz - yz * zy) / det;
			res.xy = (xz * zy - xy * zz) / det;
			res.xz = (xy * yz - xz * yy) / det;
			res.yx = (yz * zx - yx * zz) / det;
			res.yy = (xx * zz - xz * zx) / det;
			res.yz = (xz * yx - xx * yz) / det;
			res.zx = (yx * zy - yy * zx) / det;
			res.zy = (xy * zx - xx * zy) / det;
			res.zz = (xx * yy - xy * yx) / det;
			return res;
		}

		inline const Vector<3, Element> rowX() const { return Vector<3, Element>(xx, xy, xz); }
		inline const Vector<3, Element> rowY() const { return Vector<3, Element>(yx, yy, yz); }
		inline const Vector<3, Element> rowZ() const { return Vector<3, Element>(zx, zy, zz); }

		inline const Vector<3, Element> colX() const { return Vector<3, Element>(xx, yx, zx); }
		inline const Vector<3, Element> colY() const { return Vector<3, Element>(xy, yy, zy); }
		inline const Vector<3, Element> colZ() const { return Vector<3, Element>(xz, yz, zz); }

		const std::string toString() const;
	};

	template<class Element>
	class Matrix<2, Element>
	{
	public:
		Element xx, yx;
		Element xy, yy;

	public:
		inline Matrix()
			: xx(0), yx(0)
			, xy(0), yy(0)
		{
		}

		inline const Matrix operator+(const Matrix & m) const
		{
			Matrix res;
			res.xx = xx + m.xx; res.yx = yx + m.yx;
			res.xy = xy + m.xy; res.yy = yy + m.yy;
			return res;
		}

		inline const Matrix operator-(const Matrix & m) const
		{
			Matrix res;
			res.xx = xx - m.xx; res.yx = yx - m.yx;
			res.xy = xy - m.xy; res.yy = yy - m.yy;
			return res;
		}

		inline const Matrix operator*(const Matrix & m) const
		{
			Matrix res;
			res.xx = xx * m.xx + xy * m.yx;
			res.yx = yx * m.xx + yy * m.yx;
			res.xy = xx * m.xy + xy * m.yy;
			res.yy = yx * m.xy + yy * m.yy;
			return res;
		}

		inline const Vector<2, Element> operator*(const Vector<2, Element> & v) const
		{
			return Vector<2, Element>(
				xx * v.x + xy * v.y,
				yx * v.x + yy * v.y);
		}

		inline Element determinant() const
		{
			return xx * yy - xy * yx;
		}

		inline const Matrix inverse() const
		{
			Element det = determinant();
			Matrix res;
			res.xx = yy / det;
			res.xy = -xy / det;
			res.yx = -yx / det;
			res.yy = xx / det;
			return res;
		}

		inline const Vector<2, Element> rowX() const { return Vector<2, Element>(xx, xy); }
		inline const Vector<2, Element> rowY() const { return Vector<2, Element>(yx, yy); }

		inline const Vector<2, Element> colX() const { return Vector<2, Element>(xx, yx); }
		inline const Vector<2, Element> colY() const { return Vector<2, Element>(xy, yy); }

		const std::string toString() const;
	};


	typedef Matrix<2, float>  Matrix2f;
	typedef Matrix<2, double> Matrix2d;
	typedef Matrix<3, float>  Matrix3f;
	typedef Matrix<3, double> Matrix3d;
	typedef Matrix<4, float>  Matrix4f;
	typedef Matrix<4, double> Matrix4d;
}
