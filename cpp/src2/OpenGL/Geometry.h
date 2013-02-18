#pragma once

#include "Algebra.h"

namespace OpenGL
{
	class MatrixUtil
	{
	public:
		template<class Element> static const Matrix<4, Element> translationMatrix(const Vector<3, Element> &);
		template<class Element> static const Matrix<4, Element> scaleMatrix(Element scale);
		template<class Element> static const Matrix<4, Element> cylinderMatrix(const Vector<3, Element> &, const Vector<3, Element> &, Element);
		template<class Element> static const Matrix<4, Element> perspectiveMatrix(Element, Element, Element, Element);
		template<class Element> static const Matrix<4, Element> orthoMatrix(Element, Element, Element, Element, Element, Element);
		template<class Element> static const Matrix<4, Element> lookAtMatrix(const Vector<3, Element> &, const Vector<3, Element> &, const Vector<3, Element> &);
	};
}
