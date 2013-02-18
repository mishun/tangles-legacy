#pragma once

#include <vector>
#include <string>
#include "GL/gl.h"
#include "Color.h"
#include "Algebra/Math.h"
#include "GLMatrix.h"

namespace Kino
{
	// GL_ARB_vertex_buffer_object
	class GLVBO
	{
	public:
		GLVBO();
		~GLVBO();

	public:
		const PFNGLBINDBUFFERARBPROC           glBindBuffer;
		const PFNGLDELETEBUFFERSARBPROC        glDeleteBuffers;
		const PFNGLGENBUFFERSARBPROC           glGenBuffers;
		const PFNGLISBUFFERARBPROC             glIsBuffer;
		const PFNGLBUFFERDATAARBPROC           glBufferData;
		const PFNGLBUFFERSUBDATAARBPROC        glBufferSubData;
		const PFNGLGETBUFFERSUBDATAARBPROC     glGetBufferSubData;
		const PFNGLMAPBUFFERARBPROC            glMapBuffer;
		const PFNGLUNMAPBUFFERARBPROC          glUnmapBuffer;
		const PFNGLGETBUFFERPARAMETERIVARBPROC glGetBufferParameteriv;
		const PFNGLGETBUFFERPOINTERVARBPROC    glGetBufferPointerv;
	};

	class GLFBO
	{
	public:
		GLFBO();
		~GLFBO();

	public:
		const PFNGLISRENDERBUFFEREXTPROC                      glIsRenderbuffer;
		const PFNGLBINDRENDERBUFFEREXTPROC                    glBindRenderbuffer;
		const PFNGLDELETERENDERBUFFERSEXTPROC                 glDeleteRenderbuffers;
		const PFNGLGENRENDERBUFFERSEXTPROC                    glGenRenderbuffers;
		const PFNGLRENDERBUFFERSTORAGEEXTPROC                 glRenderbufferStorage;
		const PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC          glGetRenderbufferParameteriv;
		const PFNGLISFRAMEBUFFEREXTPROC                       glIsFramebuffer;
		const PFNGLBINDFRAMEBUFFEREXTPROC                     glBindFramebuffer;
		const PFNGLDELETEFRAMEBUFFERSEXTPROC                  glDeleteFramebuffers;
		const PFNGLGENFRAMEBUFFERSEXTPROC                     glGenFramebuffers;
		const PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC              glCheckFramebufferStatus;
		const PFNGLFRAMEBUFFERTEXTURE1DEXTPROC                glFramebufferTexture1D;
		const PFNGLFRAMEBUFFERTEXTURE2DEXTPROC                glFramebufferTexture2D;
		const PFNGLFRAMEBUFFERTEXTURE3DEXTPROC                glFramebufferTexture3D;
		const PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC             glFramebufferRenderbuffer;
		const PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC glGetFramebufferAttachmentParameteriv;
		const PFNGLGENERATEMIPMAPEXTPROC                      glGenerateMipmap;
	};

	// GL_ARB_shader_objects
	class GLShaderObject
	{
	public:
		GLShaderObject();
		~GLShaderObject();

	public:
		const PFNGLDELETEOBJECTARBPROC         glDeleteObject;
		const PFNGLGETHANDLEARBPROC            glGetHandle;
		const PFNGLDETACHOBJECTARBPROC         glDetachObject;
		const PFNGLCREATESHADEROBJECTARBPROC   glCreateShaderObject;
		const PFNGLSHADERSOURCEARBPROC         glShaderSource;
		const PFNGLCOMPILESHADERARBPROC        glCompileShader;
		const PFNGLCREATEPROGRAMOBJECTARBPROC  glCreateProgramObject;
		const PFNGLATTACHOBJECTARBPROC         glAttachObject;
		const PFNGLLINKPROGRAMARBPROC          glLinkProgram;
		const PFNGLUSEPROGRAMOBJECTARBPROC     glUseProgramObject;
		const PFNGLVALIDATEPROGRAMARBPROC      glValidateProgram;
		const PFNGLUNIFORM1FARBPROC            glUniform1f;
		const PFNGLUNIFORM2FARBPROC            glUniform2f;
		const PFNGLUNIFORM3FARBPROC            glUniform3f;
		const PFNGLUNIFORM4FARBPROC            glUniform4f;
		const PFNGLUNIFORM1IARBPROC            glUniform1i;
		const PFNGLUNIFORM2IARBPROC            glUniform2i;
		const PFNGLUNIFORM3IARBPROC            glUniform3i;
		const PFNGLUNIFORM4IARBPROC            glUniform4i;
		const PFNGLUNIFORM1FVARBPROC           glUniform1fv;
		const PFNGLUNIFORM2FVARBPROC           glUniform2fv;
		const PFNGLUNIFORM3FVARBPROC           glUniform3fv;
		const PFNGLUNIFORM4FVARBPROC           glUniform4fv;
		const PFNGLUNIFORM1IVARBPROC           glUniform1iv;
		const PFNGLUNIFORM2IVARBPROC           glUniform2iv;
		const PFNGLUNIFORM3IVARBPROC           glUniform3iv;
		const PFNGLUNIFORM4IVARBPROC           glUniform4iv;
		const PFNGLUNIFORMMATRIX2FVARBPROC     glUniformMatrix2fv;
		const PFNGLUNIFORMMATRIX3FVARBPROC     glUniformMatrix3fv;
		const PFNGLUNIFORMMATRIX4FVARBPROC     glUniformMatrix4fv;
		const PFNGLGETOBJECTPARAMETERFVARBPROC glGetObjectParameterfv;
		const PFNGLGETOBJECTPARAMETERIVARBPROC glGetObjectParameteriv;
		const PFNGLGETINFOLOGARBPROC           glGetInfoLog;
		const PFNGLGETATTACHEDOBJECTSARBPROC   glGetAttachedObjects;
		const PFNGLGETUNIFORMLOCATIONARBPROC   glGetUniformLocation;
		const PFNGLGETACTIVEUNIFORMARBPROC     glGetActiveUniform;
		const PFNGLGETUNIFORMFVARBPROC         glGetUniformfv;
		const PFNGLGETUNIFORMIVARBPROC         glGetUniformiv;
		const PFNGLGETSHADERSOURCEARBPROC      glGetShaderSource;
	};

	class GLFog
	{
	public:
		void enable() const;
		void disable() const;
		void mode(GLuint) const;
		void color(const Color &) const;
		void range(float, float) const;
		void density(float) const;
	};

	class GLCaller
	{
	public:
		GLCaller();
		~GLCaller();
		void loadExtensions();

		void glClearColor(const Color &) const;
		void glVertex(const Vector3d &) const;
		void glNormal(const Vector3d &) const;
		void glBegin(GLuint) const;
		void glEnd() const;
		void glColor(const Color &) const;

	public:
		GLVBO & vbo() const { return *_vbo; }
		GLFBO & fbo() const { return *_fbo; }
		GLShaderObject & so() const { return *_so; }

	public:
		class Blend
		{
		public:
			void enable() const;
			void disable() const;
			void func(GLuint, GLuint) const;
		};

	public:
		GLMatrix matrix;
		Blend blend;
		GLFog fog;

	protected:
		GLVBO * _vbo;
		GLFBO * _fbo;
		GLShaderObject * _so;

	public:
		static GLCaller & gl() { return *_instance; }

	private:
		static GLCaller * _instance;
	};


	namespace GL
	{
		class GLIncopyable
		{
		public:
			GLIncopyable()
			{
			}

		private:
			GLIncopyable(const GLIncopyable &);
			const GLIncopyable & operator=(const GLIncopyable &);
		};

		class GLShader : public GLIncopyable
		{
		protected:
			GLhandleARB shader;

		protected:
			explicit GLShader(GLenum shader_type)
			{
				shader = GLCaller::gl().so().glCreateShaderObject(shader_type);
			}

		public:
			~GLShader()
			{
				GLCaller::gl().so().glDeleteObject(shader);
			}

			GLhandleARB get() const
			{
				return shader;
			}

			void shaderSource(const std::string &);
			void shaderFile(const std::string &);
			void compileShader();
			bool isCompiled() const;
			const std::string getInfoLog() const;
		};

		class GLVertexShader : public GLShader
		{
		public:
			GLVertexShader()
				: GLShader(GL_VERTEX_SHADER_ARB)
			{
			}
		};

		class GLFragmentShader : public GLShader
		{
		public:
			GLFragmentShader()
				: GLShader(GL_FRAGMENT_SHADER_ARB)
			{
			}
		};

		class GLProgram : public GLIncopyable
		{
		protected:
			GLhandleARB program;

		public:
			GLProgram()
			{
				program = GLCaller::gl().so().glCreateProgramObject();
			}

			~GLProgram()
			{
				GLCaller::gl().so().glDeleteObject(program);
			}

			GLhandleARB get() const
			{
				return program;
			}

			void attachObject(const GLShader &);
			void detachObject(const GLShader &);
			void linkProgram();
			bool isLinked() const;
			const std::string getInfoLog() const;
			void use() const;
			GLint getUniformLocation(const char *) const;
		};


		class GLVertexBufferObject : public GLIncopyable
		{
		protected:
			GLuint vbo;

		public:
			GLVertexBufferObject()
			{
				GLCaller::gl().vbo().glGenBuffers(1, &vbo);
			}

			~GLVertexBufferObject()
			{
				GLCaller::gl().vbo().glDeleteBuffers(1, &vbo);
			}

			GLuint get() const
			{
				return vbo;
			}

			void bind() const
			{
				GLCaller::gl().vbo().glBindBuffer(GL_ARRAY_BUFFER_ARB, vbo);
			}
		};


		class GLFrameBufferObject
		{
		protected:
			GLuint fbo;

		public:
			GLFrameBufferObject()
			{
				GLCaller::gl().fbo().glGenFramebuffers(1, &fbo);
			}

			~GLFrameBufferObject()
			{
				GLCaller::gl().fbo().glDeleteFramebuffers(1, &fbo);
			}

			GLuint get() const
			{
				return fbo;
			}

			void bind() const
			{
				GLCaller::gl().fbo().glBindFramebuffer(GL_FRAMEBUFFER_EXT, fbo);
			}
		};
	}
}

#include "Texture.h"
#include "VertexBufferObject.h"
#include "FrameBufferObject.h"
