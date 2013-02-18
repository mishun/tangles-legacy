#include <cmath>
#include "Geometry.h"

namespace Kino
{
	const double Geometry::pi = acos(-1.0);

	template<class Element>
	const Matrix<4, Element> Geometry::translationMatrix(const Vector<3, Element> & offset)
	{
		Matrix<4, Element> matrix;
		matrix.xx = 1;
		matrix.yy = 1;
		matrix.zz = 1;
		matrix.ww = 1;
		matrix.xw = offset.x;
		matrix.yw = offset.y;
		matrix.zw = offset.z;
		return matrix;
	}

	template<class Element>
	const Matrix<4, Element> Geometry::scaleMatrix(Element scale)
	{
		Matrix<4, Element> matrix;
		matrix.xx = scale;
		matrix.yy = scale;
		matrix.zz = scale;
		matrix.ww = 1;
		return matrix;
	}

	template<class Element>
	const Matrix<4, Element> Geometry::cylinderMatrix(const Vector<3, Element> & begin, const Vector<3, Element> & end, Element radius)
	{
		Matrix<4, Element> matrix;

		matrix.xx = 1.0;
		matrix.yy = 1.0;
		matrix.zz = 1.0;
		matrix.ww = 1.0;

		matrix.xw = begin.x;
		matrix.yw = begin.y;
		matrix.zw = begin.z;

		Vector<3, Element> delta = end - begin;

		matrix.xz = delta.x;
		matrix.yz = delta.y;
		matrix.zz = delta.z;

		Vector<3, Element> ort;
		if(fabs(delta.y) > fabs(delta.z))
			ort = Vector<3, Element>(-delta.y, delta.x, 0.0).normalized() * radius;
		else
			ort = Vector<3, Element>(-delta.z, 0.0, delta.x).normalized() * radius;

		Vector<3, Element> cross = (ort ^ delta).normalized() * radius;

		matrix.xy = ort.x;
		matrix.yy = ort.y;
		matrix.zy = ort.z;

		matrix.xx = cross.x;
		matrix.yx = cross.y;
		matrix.zx = cross.z;

		return matrix;
	}

	template<class Element>
	const Matrix<4, Element> Geometry::perspectiveMatrix(Element fovy, Element aspect, Element near, Element far)
	{
		fovy = pi * fovy / 180.0;
		Element f = 1.0 / tan(0.5 * fovy);

		Matrix<4, Element> matrix;
		matrix.xx = f / aspect;
		matrix.yy = f;
		matrix.zz = (near + far) / (near - far);
		matrix.wz = -1.0;
		matrix.zw = 2.0 * far * near / (near - far);
		return matrix;
	}

	template<class Element> 
	const Matrix<4, Element> Geometry::orthoMatrix(Element left, Element right, Element bottom, Element top, Element near, Element far)
	{
		Matrix<4, Element> matrix;
		matrix.xx = 2.0 / (right - left);
		matrix.yy = 2.0 / (top - bottom);
		matrix.zz = -2.0 / (far - near);
		matrix.xw = -(right + left) / (right - left);
		matrix.yw = -(top + bottom) / (top - bottom);
		matrix.zw = -(far + near) / (far - near);
		matrix.ww = 1.0;
		return matrix;
	}

	template<class Element> 
	const Matrix<4, Element> Geometry::lookAtMatrix(const Vector<3, Element> & position, const Vector<3, Element> & _forward, const Vector<3, Element> & _up)
	{
		Vector<3, Element> f = _forward.normalized();
		Vector<3, Element> up = _up.normalized();

		Vector<3, Element> s = f ^ up;
		Vector<3, Element> u = s ^ f;

		Matrix<4, Element> rot;
		rot.xx = s.x,  rot.xy = s.y,  rot.xz = s.z;
		rot.yx = u.x,  rot.yy = u.y,  rot.yz = u.z;
		rot.zx = -f.x, rot.zy = -f.y, rot.zz = -f.z;
		rot.ww = 1.0;

		Matrix<4, Element> trans;
		trans.xx = 1.0;
		trans.yy = 1.0;
		trans.zz = 1.0;
		trans.ww = 1.0;
		trans.xw = -position.x, trans.yw = -position.y, trans.zw = -position.z;
		return rot * trans;
	}


	template const Matrix4f Geometry::translationMatrix<float>(const Vector3f &);
	template const Matrix4d Geometry::translationMatrix<double>(const Vector3d &);

	template const Matrix4f Geometry::scaleMatrix<float>(float);
	template const Matrix4d Geometry::scaleMatrix<double>(double);

	template const Matrix4f Geometry::cylinderMatrix(const Vector3f &, const Vector3f &, float);
	template const Matrix4d Geometry::cylinderMatrix(const Vector3d &, const Vector3d &, double);

	template const Matrix4f Geometry::perspectiveMatrix<float>(float, float, float, float);
	template const Matrix4d Geometry::perspectiveMatrix<double>(double, double, double, double);

	template const Matrix4f Geometry::orthoMatrix<float>(float, float, float, float, float, float);
	template const Matrix4d Geometry::orthoMatrix<double>(double, double, double, double, double, double);

	template const Matrix4f Geometry::lookAtMatrix<float>(const Vector3f &, const Vector3f &, const Vector3f &);
	template const Matrix4d Geometry::lookAtMatrix<double>(const Vector3d &, const Vector3d &, const Vector3d &);
}
