#pragma once

//#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>
#include <GL/glext.h>

namespace Kino
{
	namespace GL
	{
		class GLTexture
		{
		protected:
			GLuint texture;

		public:
			GLTexture()
			{
				glGenTextures(1, &texture);
			}

			~GLTexture()
			{
				glDeleteTextures(1, &texture);
			}

			GLuint get() const
			{
				return texture;
			}

		private:
			GLTexture(const GLTexture &);
			GLTexture & operator=(const GLTexture &);
		};
	}
}
