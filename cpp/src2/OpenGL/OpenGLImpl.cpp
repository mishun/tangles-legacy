#include <GL/gl.h>
#include "OpenGLImpl.h"

using namespace OpenGL;

void OpenGLImpl::viewport(size_t a, size_t b, size_t c, size_t d)
{
	glViewport(a, b, c, d);
}

void OpenGLImpl::loadProjectionMatrix(const Matrix4f & m)
{
	glMatrixMode(GL_PROJECTION);
	glLoadMatrixf(m.data());
}

void OpenGLImpl::loadModelViewMatrix(const Matrix4f & m)
{
	glMatrixMode(GL_MODELVIEW);
	glLoadMatrixf(m.data());
}


void OpenGLImpl::enableDepthTest()
{
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LEQUAL);
}

void OpenGLImpl::disableDepthTest()
{
	glDisable(GL_DEPTH_TEST);
}

void OpenGLImpl::enableBlend()
{
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

void OpenGLImpl::disableBlend()
{
	glDisable(GL_BLEND);
}


void OpenGLImpl::clearDepth(float d)
{
	glClearDepth(d);
}

void OpenGLImpl::clearColor(const Color & c)
{
	glClearColor(c.r, c.g, c.b, c.a);
}


void OpenGLImpl::beginLines()
{
	glBegin(GL_LINES);
}

void OpenGLImpl::beginTriangles()
{
	glBegin(GL_TRIANGLES);
}

void OpenGLImpl::end()
{
	glEnd();
}

void OpenGLImpl::color(const Color & c)
{
	glColor4f(c.r, c.g, c.b, c.a);
}

void OpenGLImpl::vertex(const Vector3f & v)
{
	glVertex3f(v.x, v.y, v.z);
}

void OpenGLImpl::vertex(const Vector3d & v)
{
	glVertex3d(v.x, v.y, v.z);
}

void OpenGLImpl::normal(const Vector3f & n)
{
	glNormal3f(n.x, n.y, n.z);
}

void OpenGLImpl::normal(const Vector3d & n)
{
	glNormal3d(n.x, n.y, n.z);
}
