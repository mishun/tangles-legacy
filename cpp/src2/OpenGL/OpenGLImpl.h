#pragma once

#include "OpenGL.h"

namespace OpenGL
{
	class OpenGLImpl : public OpenGL::OpenGL
	{
	public:
		virtual void viewport(size_t, size_t, size_t, size_t);
		virtual void loadProjectionMatrix(const Matrix4f &);
		virtual void loadModelViewMatrix(const Matrix4f &);

		virtual void enableDepthTest();
		virtual void disableDepthTest();
		virtual void enableBlend();
		virtual void disableBlend();

		virtual void clearDepth(float);
		virtual void clearColor(const Color &);

		virtual void beginLines();
		virtual void beginTriangles();
		virtual void end();
		virtual void color(const Color &);
		virtual void vertex(const Vector3f &);
		virtual void vertex(const Vector3d &);
		virtual void normal(const Vector3f &);
		virtual void normal(const Vector3d &);
	};
}
