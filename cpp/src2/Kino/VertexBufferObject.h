#pragma once

#include "SharedObject.h"

namespace Kino
{
	class VertexBufferObjectImpl : public SharedObject
	{
		friend class VertexBufferObject;

	protected:
		VertexBufferObjectImpl();
		virtual ~VertexBufferObjectImpl();
		virtual void bind() const;
		virtual void bufferData(const void *, size_t) const;

	protected:
		GL::GLVertexBufferObject vbo;
	};

	class VertexBufferObject : public SharedPointer<VertexBufferObjectImpl>
	{
	public:
		VertexBufferObject()
			: SharedPointer<VertexBufferObjectImpl>(0)
		{
		}

		inline void bind() const
		{
			get().bind();
		}

		inline void bufferData(const void * data, size_t size) const
		{
			get().bufferData(data, size);
		}

	protected:
		VertexBufferObject(VertexBufferObjectImpl * impl)
			: SharedPointer<VertexBufferObjectImpl>(impl)
		{
		}

	public:
		static VertexBufferObject create()
		{
			return VertexBufferObject(new VertexBufferObjectImpl());
		}
	};
}
