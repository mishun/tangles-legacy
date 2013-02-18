#pragma once

#include "SharedObject.h"

namespace Kino
{

	class TextureImpl : public SharedObject
	{
	public:
		virtual void bind() = 0;
		virtual void copyImage(int, int, int, int, size_t, size_t, int) = 0;
		virtual void copySubImage(int, int, int, int, int, size_t, size_t) = 0;
		virtual GLuint getTexture() = 0;

	public:
		static TextureImpl * create(size_t, size_t);
	};

	class Texture : public SharedPointer<TextureImpl>
	{
	public:
		Texture()
			: SharedPointer<TextureImpl>()
		{
		}

		void bind() const
		{
			get().bind();
		}

		void copyImage(int level, int internalformat, int x, int y, size_t width, size_t height, int border)
		{
			get().copyImage(level, internalformat, x, y, width, height, border);
		}

		void copySubImage(int level, int xoffset, int yoffset, int x, int y, size_t width, size_t height) const
		{
			get().copySubImage(level, xoffset, yoffset, x, y, width, height);
		}

		GLuint getTexture() const
		{
			return get().getTexture();
		}

	protected:
		Texture(TextureImpl * impl)
			: SharedPointer<TextureImpl>(impl)
		{
		}

	public:
		static Texture create(size_t width, size_t height)
		{
			return Texture(TextureImpl::create(width, height));
		}
	};
}
