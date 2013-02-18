#include <sstream>
#include "Matrix.h"

namespace OpenGL
{
	template<class Element>
	Element Matrix<4, Element>::determinant() const
	{
		return	xw * yz * zy * wx - xz * yw * zy * wx - xw * yy * zz * wx + xy * yw * zz * wx +
			xz * yy * zw * wx - xy * yz * zw * wx - xw * yz * zx * wy + xz * yw * zx * wy +
			xw * yx * zz * wy - xx * yw * zz * wy - xz * yx * zw * wy + xx * yz * zw * wy +
			xw * yy * zx * wz - xy * yw * zx * wz - xw * yx * zy * wz + xx * yw * zy * wz +
			xy * yx * zw * wz - xx * yy * zw * wz - xz * yy * zx * ww + xy * yz * zx * ww +
			xz * yx * zy * ww - xx * yz * zy * ww - xy * yx * zz * ww + xx * yy * zz * ww;
	}

	template<class Element>
	const Matrix<4, Element> Matrix<4, Element>::inverse() const
	{
		Element det = determinant();
		Matrix<4, Element> res;
		res.xx = (yz * zw * wy - yw * zz * wy + yw * zy * wz - yy * zw * wz - yz * zy * ww + yy * zz * ww) / det;
		res.xy = (xw * zz * wy - xz * zw * wy - xw * zy * wz + xy * zw * wz + xz * zy * ww - xy * zz * ww) / det;
		res.xz = (xz * yw * wy - xw * yz * wy + xw * yy * wz - xy * yw * wz - xz * yy * ww + xy * yz * ww) / det;
		res.xw = (xw * yz * zy - xz * yw * zy - xw * yy * zz + xy * yw * zz + xz * yy * zw - xy * yz * zw) / det;
		res.yx = (yw * zz * wx - yz * zw * wx - yw * zx * wz + yx * zw * wz + yz * zx * ww - yx * zz * ww) / det;
		res.yy = (xz * zw * wx - xw * zz * wx + xw * zx * wz - xx * zw * wz - xz * zx * ww + xx * zz * ww) / det;
		res.yz = (xw * yz * wx - xz * yw * wx - xw * yx * wz + xx * yw * wz + xz * yx * ww - xx * yz * ww) / det;
		res.yw = (xz * yw * zx - xw * yz * zx + xw * yx * zz - xx * yw * zz - xz * yx * zw + xx * yz * zw) / det;
		res.zx = (yy * zw * wx - yw * zy * wx + yw * zx * wy - yx * zw * wy - yy * zx * ww + yx * zy * ww) / det;
		res.zy = (xw * zy * wx - xy * zw * wx - xw * zx * wy + xx * zw * wy + xy * zx * ww - xx * zy * ww) / det;
		res.zz = (xy * yw * wx - xw * yy * wx + xw * yx * wy - xx * yw * wy - xy * yx * ww + xx * yy * ww) / det;
		res.zw = (xw * yy * zx - xy * yw * zx - xw * yx * zy + xx * yw * zy + xy * yx * zw - xx * yy * zw) / det;
		res.wx = (yz * zy * wx - yy * zz * wx - yz * zx * wy + yx * zz * wy + yy * zx * wz - yx * zy * wz) / det;
		res.wy = (xy * zz * wx - xz * zy * wx + xz * zx * wy - xx * zz * wy - xy * zx * wz + xx * zy * wz) / det;
		res.wz = (xz * yy * wx - xy * yz * wx - xz * yx * wy + xx * yz * wy + xy * yx * wz - xx * yy * wz) / det;
		res.ww = (xy * yz * zx - xz * yy * zx + xz * yx * zy - xx * yz * zy - xy * yx * zz + xx * yy * zz) / det;
		return res;
	}

	template<class Element>
	const std::string Matrix<4, Element>::toString() const
	{
		std::ostringstream str;
		str << "[[" << xx << ' ' << xy << ' ' << xz << ' ' << xw << "] ["
			<< yx << ' ' << yy << ' ' << yz << ' ' << yw << "] ["
			<< yx << ' ' << yy << ' ' << yz << ' ' << zw << "] ["
			<< wx << ' ' << wy << ' ' << wz << ' ' << ww << "]]";
		return str.str();
	}

	template<class Element>
	const std::string Matrix<3, Element>::toString() const
	{
		std::ostringstream str;
		str << "[[" << xx << ' ' << xy << ' ' << xz << "] ["
			<< yx << ' ' << yy << ' '<< yz << "] ["
			<< zx << ' ' << zy << ' ' << zz << "]]";
		return str.str();
	}

	template<class Element>
	const std::string Matrix<2, Element>::toString() const
	{
		std::ostringstream str;
		str << "[[" << xx << ' ' << xy << "] ["
			<< yx << ' ' << yy << "]]";
		return str.str();
	}


	template float Matrix<4, float>::determinant() const;
	template double Matrix<4, double>::determinant() const;

	template const Matrix<4, float> Matrix<4, float>::inverse() const;
	template const Matrix<4, double> Matrix<4, double>::inverse() const;

	template const std::string Matrix<4, float>::toString() const;
	template const std::string Matrix<4, double>::toString() const;

	template const std::string Matrix<3, float>::toString() const;
	template const std::string Matrix<3, double>::toString() const;

	template const std::string Matrix<2, float>::toString() const;
	template const std::string Matrix<2, double>::toString() const;
}
