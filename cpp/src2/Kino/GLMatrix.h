#pragma once

#include "Algebra/Math.h"

namespace Kino
{
	class GLMatrix
	{
	public:
		const Matrix4f getModelViewf() const;
		const Matrix4d getModelView() const;
		const Matrix4f getProjectionf() const;
		const Matrix4d getProjection() const;
		void setModelView(const Matrix4f &) const;
		void setModelView(const Matrix4d &) const;
		void setProjection(const Matrix4f &) const;
		void setProjection(const Matrix4d &) const;
		void pushModelView() const;
		void pushProjection() const;
		void popModelView() const;
		void popProjection() const;
		void multiplyModelView(const Matrix4d &) const;
		void identityModelView() const;
	};
}
