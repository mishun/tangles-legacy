#pragma once

#include "SharedObject.h"

namespace Kino
{
	class FrameBufferObjectImpl : public SharedObject
	{
		friend class FrameBufferObject;

	protected:
		FrameBufferObjectImpl();
		virtual ~FrameBufferObjectImpl();

	protected:
		GL::GLFrameBufferObject fbo;
	};

	class FrameBufferObject : public SharedPointer<FrameBufferObjectImpl>
	{
	public:
		FrameBufferObject()
			: SharedPointer<FrameBufferObjectImpl>(0)
		{
		}

	protected:
		FrameBufferObject(FrameBufferObjectImpl * impl)
			: SharedPointer<FrameBufferObjectImpl>(impl)
		{
		}

	public:
		static FrameBufferObject create()
		{
			return FrameBufferObject(new FrameBufferObjectImpl());
		}
	};
}
