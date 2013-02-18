#pragma once

#include "Vector.h"
#include "Matrix.h"

namespace OpenGL
{
	template<class Element>
	inline const Vector<4, Element> operator*(const Vector<4, Element> & v, const Matrix<4, Element> & m)
	{
		return Vector<4, Element>(
			v.x * m.xx + v.y * m.yx + v.z * m.zx + v.w * m.wx,
			v.x * m.xy + v.y * m.yy + v.z * m.zy + v.w * m.wy,
			v.x * m.xz + v.y * m.yz + v.z * m.zz + v.w * m.wz,
			v.x * m.xw + v.y * m.yw + v.z * m.zw + v.w * m.ww);
	}

	template<class Element>
	inline const Vector<3, Element> operator*(const Vector<3, Element> & v, const Matrix<3, Element> & m)
	{
		return Vector<3, Element>(
			v.x * m.xx + v.y * m.yx + v.z * m.zx,
			v.x * m.xy + v.y * m.yy + v.z * m.zy,
			v.x * m.xz + v.y * m.yz + v.z * m.zz);
	}

	template<class Element>
	inline const Vector<2, Element> operator*(const Vector<2, Element> & v, const Matrix<2, Element> & m)
	{
		return Vector<2, Element>(
			v.x * m.xx + v.y * m.yx,
			v.x * m.xy + v.y * m.yy);
	}

	template<class Element>
	inline const Vector<3, Element> operator*(Element m, const Vector<3, Element> & v)
	{
		return Vector<3, Element>(m * v.x, m * v.y, m * v.z);
	}

	template<class Element>
	inline const Vector<2, Element> operator*(Element m, const Vector<3, Element> & v)
	{
		return Vector<2, Element>(m * v.x, m * v.y);
	}
}
