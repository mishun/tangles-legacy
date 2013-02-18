#include "GLCaller.h"
#include "VertexBufferObject.h"

namespace Kino
{
	VertexBufferObjectImpl::VertexBufferObjectImpl()
	{
		//GLCaller::gl().vbo().glGenBuffers(1, &vbo);
	}

	VertexBufferObjectImpl::~VertexBufferObjectImpl()
	{
		//GLCaller::gl().vbo().glDeleteBuffers(1, &vbo);
	}

	void VertexBufferObjectImpl::bind() const
	{
		//GLCaller::gl().vbo().glBindBuffer(GL_ARRAY_BUFFER_ARB, vbo);
		vbo.bind();
	}

	void VertexBufferObjectImpl::bufferData(const void * data, size_t size) const
	{
		//GLCaller::gl().vbo().glBindBuffer(GL_ARRAY_BUFFER_ARB, vbo);
		vbo.bind();
		GLCaller::gl().vbo().glBufferData(GL_ARRAY_BUFFER_ARB, size, data, GL_STATIC_DRAW_ARB);
	}
}
