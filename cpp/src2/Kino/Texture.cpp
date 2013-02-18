#include "GLCaller.h"
#include "Texture.h"

namespace Kino
{
	class Texture2D : public TextureImpl
	{
	protected:
		GL::GLTexture texture;

	public:
		Texture2D(size_t, size_t);
		virtual ~Texture2D();

		virtual void bind();
		virtual void copyImage(int, int, int, int, size_t, size_t, int);
		virtual void copySubImage(int, int, int, int, int, size_t, size_t);
		virtual GLuint getTexture();
	};

	Texture2D::Texture2D(size_t width, size_t height)
	{
		glBindTexture(GL_TEXTURE_2D, texture.get());
		glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, width, height, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, NULL);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	}

	Texture2D::~Texture2D()
	{
	}

	void Texture2D::bind()
	{
		glBindTexture(GL_TEXTURE_2D, texture.get());
	}

	void Texture2D::copyImage(int level, int internalformat, int x, int y, size_t width, size_t height, int border)
	{
		glBindTexture(GL_TEXTURE_2D, texture.get());
		glCopyTexImage2D(GL_TEXTURE_2D, level, internalformat, x, y, width, height, border);
 	}

	void Texture2D::copySubImage(int level, int xoffset, int yoffset, int x, int y, size_t width, size_t height)
	{
		glBindTexture(GL_TEXTURE_2D, texture.get());
		glCopyTexSubImage2D(GL_TEXTURE_2D, level, xoffset, yoffset, x, y, width, height);
	}

	GLuint Texture2D::getTexture()
	{
		return texture.get();
	}


	TextureImpl * TextureImpl::create(size_t width, size_t height)
	{
		return new Texture2D(width, height);
	}
}
