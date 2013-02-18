#include "GL/gl.h"
#include "GLMatrix.h"

namespace Kino
{
	const Matrix4f GLMatrix::getModelViewf() const
	{
		Matrix4f modelview;
		::glGetFloatv(GL_MODELVIEW_MATRIX, (float *)&modelview);
		return modelview;
	}

	const Matrix4d GLMatrix::getModelView() const
	{
		Matrix4d modelview;
		::glGetDoublev(GL_MODELVIEW_MATRIX, (double *)&modelview);
		return modelview;
	}

	const Matrix4f GLMatrix::getProjectionf() const
	{
		Matrix4f projection;
		::glGetFloatv(GL_PROJECTION_MATRIX, (float *)&projection);
		return projection;
	}

	const Matrix4d GLMatrix::getProjection() const
	{
		Matrix4d projection;
		::glGetDoublev(GL_PROJECTION_MATRIX, (double *)&projection);
		return projection;
	}

	void GLMatrix::setModelView(const Matrix4f & matrix) const
	{
		::glLoadMatrixf(matrix.data());
	}

	void GLMatrix::setModelView(const Matrix4d & matrix) const
	{
		::glLoadMatrixd(matrix.data());
	}

	void GLMatrix::setProjection(const Matrix4f & matrix) const
	{
		::glMatrixMode(GL_PROJECTION);
		::glLoadMatrixf(matrix.data());
		::glMatrixMode(GL_MODELVIEW);
	}

	void GLMatrix::setProjection(const Matrix4d & matrix) const
	{
		::glMatrixMode(GL_PROJECTION);
		::glLoadMatrixd(matrix.data());
		::glMatrixMode(GL_MODELVIEW);
	}

	void GLMatrix::pushModelView() const
	{
		::glPushMatrix();
	}

	void GLMatrix::pushProjection() const
	{
		::glMatrixMode(GL_PROJECTION);
		::glPushMatrix();
		::glMatrixMode(GL_MODELVIEW);
	}

	void GLMatrix::popModelView() const
	{
		::glPopMatrix();
	}

	void GLMatrix::popProjection() const
	{
		::glMatrixMode(GL_PROJECTION);
		::glPopMatrix();
		::glMatrixMode(GL_MODELVIEW);
	}

	void GLMatrix::multiplyModelView(const Matrix4d & matrix) const
	{
		::glMultMatrixd(matrix.data());
	}

	void GLMatrix::identityModelView() const
	{
		::glLoadIdentity();
	}
}
