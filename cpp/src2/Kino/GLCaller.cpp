#include <iostream>
#include <fstream>
#include <sstream>
#include "GLCaller.h"
#include <windows.h>
#include <wingdi.h>

namespace Kino
{
	GLCaller * GLCaller::_instance = 0;


	GLCaller::GLCaller()
		: _vbo(0)
		, _fbo(0)
		, _so(0)
	{
		glMatrixMode(GL_MODELVIEW);
		_instance = this;
	}

	GLCaller::~GLCaller()
	{
		delete _vbo;
		delete _fbo;
		delete _so;
	}

	void GLCaller::loadExtensions()
	{
		std::cout << glGetString(GL_EXTENSIONS) << "\n\n";

		_vbo = new GLVBO();
		_fbo = new GLFBO();
		_so = new GLShaderObject();
	}

	void GLCaller::glClearColor(const Color & color) const
	{
		::glClearColor(color.r, color.g, color.b, color.a);
	}

	void GLCaller::glVertex(const Vector3d & v) const
	{
		::glVertex3d(v.x, v.y, v.z);
	}

	void GLCaller::glNormal(const Vector3d & v) const
	{
		::glNormal3d(v.x, v.y, v.z);
	}

	void GLCaller::glBegin(GLuint mode) const
	{
		::glBegin(mode);
	}

	void GLCaller::glEnd() const
	{
		::glEnd();
	}

	void GLCaller::glColor(const Color & color) const
	{
		::glColor4fv((const float *)&color);
	}


	void GLCaller::Blend::enable() const
	{
		glEnable(GL_BLEND);
	}

	void GLCaller::Blend::disable() const
	{
		glDisable(GL_BLEND);
	}

	void GLCaller::Blend::func(GLuint src, GLuint dst) const
	{
		glBlendFunc(src, dst);
	}


	void GLFog::enable() const
	{
		glEnable(GL_FOG);
		glHint(GL_FOG_HINT, GL_NICEST);
	}

	void GLFog::disable() const
	{
		glDisable(GL_FOG);
	}

	void GLFog::mode(GLuint mode) const
	{
		glFogi(GL_FOG_MODE, mode);
	}

	void GLFog::color(const Color & color) const
	{
		glFogfv(GL_FOG_COLOR, (const float *)&color);
	}

	void GLFog::range(float start, float end) const
	{
		glFogf(GL_FOG_START, start);
		glFogf(GL_FOG_END, end);
	}

	void GLFog::density(float density) const
	{
		glFogf(GL_FOG_DENSITY, density);
	}


	GLVBO::GLVBO()
		: glBindBuffer          ((PFNGLBINDBUFFERARBPROC)          wglGetProcAddress("glBindBufferARB"))
		, glDeleteBuffers       ((PFNGLDELETEBUFFERSARBPROC)       wglGetProcAddress("glDeleteBuffersARB"))
		, glGenBuffers          ((PFNGLGENBUFFERSARBPROC)          wglGetProcAddress("glGenBuffersARB"))
		, glIsBuffer            ((PFNGLISBUFFERARBPROC)            wglGetProcAddress("glIsBufferARB"))
		, glBufferData          ((PFNGLBUFFERDATAARBPROC)          wglGetProcAddress("glBufferDataARB"))
		, glBufferSubData       ((PFNGLBUFFERSUBDATAARBPROC)       wglGetProcAddress("glBufferSubDataARB"))
		, glGetBufferSubData    ((PFNGLGETBUFFERSUBDATAARBPROC)    wglGetProcAddress("glGetBufferSubDataARB"))
		, glMapBuffer           ((PFNGLMAPBUFFERARBPROC)           wglGetProcAddress("glMapBufferARB"))
		, glUnmapBuffer         ((PFNGLUNMAPBUFFERARBPROC)         wglGetProcAddress("glUnmapBufferARB"))
		, glGetBufferParameteriv((PFNGLGETBUFFERPARAMETERIVARBPROC)wglGetProcAddress("glGetBufferParameterivARB"))
		, glGetBufferPointerv   ((PFNGLGETBUFFERPOINTERVARBPROC)   wglGetProcAddress("glGetBufferPointerv"))
	{
	}

	GLVBO::~GLVBO()
	{
	}


	GLFBO::GLFBO()
		: glIsRenderbuffer                     ((PFNGLISRENDERBUFFEREXTPROC)                     wglGetProcAddress("glIsRenderbufferEXT"))
		, glBindRenderbuffer                   ((PFNGLBINDRENDERBUFFEREXTPROC)                   wglGetProcAddress("glBindRenderbufferEXT"))
		, glDeleteRenderbuffers                ((PFNGLDELETERENDERBUFFERSEXTPROC)                wglGetProcAddress("glDeleteRenderbuffersEXT"))
		, glGenRenderbuffers                   ((PFNGLGENRENDERBUFFERSEXTPROC)                   wglGetProcAddress("glGenRenderbuffersEXT"))
		, glRenderbufferStorage                ((PFNGLRENDERBUFFERSTORAGEEXTPROC)                wglGetProcAddress("glRenderbufferStorageEXT"))
		, glGetRenderbufferParameteriv         ((PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC)         wglGetProcAddress("glGetRenderbufferParameterivEXT"))
		, glIsFramebuffer                      ((PFNGLISFRAMEBUFFEREXTPROC)                      wglGetProcAddress("glIsFramebufferEXT"))
		, glBindFramebuffer                    ((PFNGLBINDFRAMEBUFFEREXTPROC)                    wglGetProcAddress("glBindFramebufferEXT"))
		, glDeleteFramebuffers                 ((PFNGLDELETEFRAMEBUFFERSEXTPROC)                 wglGetProcAddress("glDeleteFramebuffersEXT"))
		, glGenFramebuffers                    ((PFNGLGENFRAMEBUFFERSEXTPROC)                    wglGetProcAddress("glGenFramebuffersEXT"))
		, glCheckFramebufferStatus             ((PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC)             wglGetProcAddress("glCheckFramebufferStatusEXT"))
		, glFramebufferTexture1D               ((PFNGLFRAMEBUFFERTEXTURE1DEXTPROC)               wglGetProcAddress("glFramebufferTexture1DEXT"))
		, glFramebufferTexture2D               ((PFNGLFRAMEBUFFERTEXTURE2DEXTPROC)               wglGetProcAddress("glFramebufferTexture2DEXT"))
		, glFramebufferTexture3D               ((PFNGLFRAMEBUFFERTEXTURE3DEXTPROC)               wglGetProcAddress("glFramebufferTexture3DEXT"))
		, glFramebufferRenderbuffer            ((PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC)            wglGetProcAddress("glFramebufferRenderbufferEXT"))
		, glGetFramebufferAttachmentParameteriv((PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC)wglGetProcAddress("glGetFramebufferAttachmentParameterivEXT"))
		, glGenerateMipmap                     ((PFNGLGENERATEMIPMAPEXTPROC)                     wglGetProcAddress("glGenerateMipmapEXT"))
	{
	}

	GLFBO::~GLFBO()
	{
	}


	GLShaderObject::GLShaderObject()
		: glDeleteObject        ((PFNGLDELETEOBJECTARBPROC)        wglGetProcAddress("glDeleteObjectARB"))
		, glGetHandle           ((PFNGLGETHANDLEARBPROC)           wglGetProcAddress("glGetHandleARB"))
		, glDetachObject        ((PFNGLDETACHOBJECTARBPROC)        wglGetProcAddress("glDetachObjectARB"))
		, glCreateShaderObject  ((PFNGLCREATESHADEROBJECTARBPROC)  wglGetProcAddress("glCreateShaderObjectARB"))
		, glShaderSource        ((PFNGLSHADERSOURCEARBPROC)        wglGetProcAddress("glShaderSourceARB"))
		, glCompileShader       ((PFNGLCOMPILESHADERARBPROC)       wglGetProcAddress("glCompileShaderARB"))
		, glCreateProgramObject ((PFNGLCREATEPROGRAMOBJECTARBPROC) wglGetProcAddress("glCreateProgramObjectARB"))
		, glAttachObject        ((PFNGLATTACHOBJECTARBPROC)        wglGetProcAddress("glAttachObjectARB"))
		, glLinkProgram         ((PFNGLLINKPROGRAMARBPROC)         wglGetProcAddress("glLinkProgramARB"))
		, glUseProgramObject    ((PFNGLUSEPROGRAMOBJECTARBPROC)    wglGetProcAddress("glUseProgramObjectARB"))
		, glValidateProgram     ((PFNGLVALIDATEPROGRAMARBPROC)     wglGetProcAddress("glValidateProgramARB"))
		, glUniform1f           ((PFNGLUNIFORM1FARBPROC)           wglGetProcAddress("glUniform1fARB"))
		, glUniform2f           ((PFNGLUNIFORM2FARBPROC)           wglGetProcAddress("glUniform2fARB"))
		, glUniform3f           ((PFNGLUNIFORM3FARBPROC)           wglGetProcAddress("glUniform3fARB"))
		, glUniform4f           ((PFNGLUNIFORM4FARBPROC)           wglGetProcAddress("glUniform4fARB"))
		, glUniform1i           ((PFNGLUNIFORM1IARBPROC)           wglGetProcAddress("glUniform1iARB"))
		, glUniform2i           ((PFNGLUNIFORM2IARBPROC)           wglGetProcAddress("glUniform2iARB"))
		, glUniform3i           ((PFNGLUNIFORM3IARBPROC)           wglGetProcAddress("glUniform3iARB"))
		, glUniform4i           ((PFNGLUNIFORM4IARBPROC)           wglGetProcAddress("glUniform4iARB"))
		, glUniform1fv          ((PFNGLUNIFORM1FVARBPROC)          wglGetProcAddress("glUniform1fvARB"))
		, glUniform2fv          ((PFNGLUNIFORM2FVARBPROC)          wglGetProcAddress("glUniform2fvARB"))
		, glUniform3fv          ((PFNGLUNIFORM3FVARBPROC)          wglGetProcAddress("glUniform3fvARB"))
		, glUniform4fv          ((PFNGLUNIFORM4FVARBPROC)          wglGetProcAddress("glUniform4fvARB"))
		, glUniform1iv          ((PFNGLUNIFORM1IVARBPROC)          wglGetProcAddress("glUniform1ivARB"))
		, glUniform2iv          ((PFNGLUNIFORM2IVARBPROC)          wglGetProcAddress("glUniform2ivARB"))
		, glUniform3iv          ((PFNGLUNIFORM3IVARBPROC)          wglGetProcAddress("glUniform3ivARB"))
		, glUniform4iv          ((PFNGLUNIFORM4IVARBPROC)          wglGetProcAddress("glUniform4ivARB"))
		, glUniformMatrix2fv    ((PFNGLUNIFORMMATRIX2FVARBPROC)    wglGetProcAddress("glUniformMatrix2fvARB"))
		, glUniformMatrix3fv    ((PFNGLUNIFORMMATRIX3FVARBPROC)    wglGetProcAddress("glUniformMatrix3fvARB"))
		, glUniformMatrix4fv    ((PFNGLUNIFORMMATRIX4FVARBPROC)    wglGetProcAddress("glUniformMatrix4fvARB"))
		, glGetObjectParameterfv((PFNGLGETOBJECTPARAMETERFVARBPROC)wglGetProcAddress("glGetObjectParameterfvARB"))
		, glGetObjectParameteriv((PFNGLGETOBJECTPARAMETERIVARBPROC)wglGetProcAddress("glGetObjectParameterivARB"))
		, glGetInfoLog          ((PFNGLGETINFOLOGARBPROC)          wglGetProcAddress("glGetInfoLogARB"))
		, glGetAttachedObjects  ((PFNGLGETATTACHEDOBJECTSARBPROC)  wglGetProcAddress("glGetAttachedObjectsARB"))
		, glGetUniformLocation  ((PFNGLGETUNIFORMLOCATIONARBPROC)  wglGetProcAddress("glGetUniformLocationARB"))
		, glGetActiveUniform    ((PFNGLGETACTIVEUNIFORMARBPROC)    wglGetProcAddress("glGetActiveUniformARB"))
		, glGetUniformfv        ((PFNGLGETUNIFORMFVARBPROC)        wglGetProcAddress("glGetUniformfvARB"))
		, glGetUniformiv        ((PFNGLGETUNIFORMIVARBPROC)        wglGetProcAddress("glGetUniformivARB"))
		, glGetShaderSource     ((PFNGLGETSHADERSOURCEARBPROC)     wglGetProcAddress("glGetShaderSourceARB"))
	{
	}

	GLShaderObject::~GLShaderObject()
	{
	}

	namespace GL
	{
		void GLShader::shaderSource(const std::string & source)
		{
			const char * src = source.c_str();
			const GLint len = source.length();
			GLCaller::gl().so().glShaderSource(shader, 1, &src, &len);
		}

		void GLShader::shaderFile(const std::string & filename)
		{
			std::ifstream in(filename.c_str());
			std::ostringstream shader;
			while(!in.eof())
			{
				std::string line;
				getline(in, line);
				shader << line << "\n";
			}
			shaderSource(shader.str());
		}

		void GLShader::compileShader()
		{
			GLCaller::gl().so().glCompileShader(shader);
		}

		bool GLShader::isCompiled() const
		{
			GLint compiled = 0;
			GLCaller::gl().so().glGetObjectParameteriv(shader, GL_OBJECT_COMPILE_STATUS_ARB, &compiled);
			return compiled != 0;
		}

		const std::string GLShader::getInfoLog() const
		{
			GLint log_length = 0;
			GLCaller::gl().so().glGetObjectParameteriv(shader, GL_OBJECT_INFO_LOG_LENGTH_ARB, &log_length);
			std::vector<char> log(log_length);
			GLint laux;
			GLCaller::gl().so().glGetInfoLog(shader, log_length, &laux, &log[0]);
			return std::string(&log[0]);
		}


		void GLProgram::attachObject(const GLShader & shader)
		{
			GLCaller::gl().so().glAttachObject(program, shader.get());
		}

		void GLProgram::detachObject(const GLShader & shader)
		{
			GLCaller::gl().so().glDetachObject(program, shader.get());
		}

		void GLProgram::linkProgram()
		{
			GLCaller::gl().so().glLinkProgram(program);
		}

		bool GLProgram::isLinked() const
		{
			GLint linked = 0;
			GLCaller::gl().so().glGetObjectParameteriv(program, GL_OBJECT_LINK_STATUS_ARB, &linked);
			return linked != 0;
		}

		const std::string GLProgram::getInfoLog() const
		{
			GLint log_length = 0;
			GLCaller::gl().so().glGetObjectParameteriv(program, GL_OBJECT_INFO_LOG_LENGTH_ARB, &log_length);
			std::vector<char> log(log_length);
			GLint laux;
			GLCaller::gl().so().glGetInfoLog(program, log_length, &laux, &log[0]);
			return std::string(&log[0]);
		}

		void GLProgram::use() const
		{
			GLCaller::gl().so().glUseProgramObject(program);
		}

		GLint GLProgram::getUniformLocation(const char * name) const
		{
			return GLCaller::gl().so().glGetUniformLocation(program, name);
		}
	}
}
