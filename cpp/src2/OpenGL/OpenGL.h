#pragma once

#include <vector>
#include <string>

#include <GL/gl.h>
#include <GL/glext.h>

#include "Color.h"
#include "Algebra.h"
#include "Geometry.h"
#include "Vector.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <windows.h>
#include <wingdi.h>

namespace OpenGL
{
	class OpenGL
	{
	protected:
		virtual ~OpenGL() {}

	public:
		virtual void viewport(size_t, size_t, size_t, size_t) = 0;
		virtual void loadProjectionMatrix(const Matrix4f &) = 0;
		virtual void loadModelViewMatrix(const Matrix4f &) = 0;

		virtual void enableDepthTest() = 0;
		virtual void disableDepthTest() = 0;
		virtual void enableBlend() = 0;
		virtual void disableBlend() = 0;

		virtual void clearDepth(float) = 0;
		virtual void clearColor(const Color &) = 0;

		virtual void beginLines() = 0;
		virtual void beginTriangles() = 0;
		virtual void end() = 0;
		virtual void color(const Color &) = 0;
		virtual void vertex(const Vector3f &) = 0;
		virtual void vertex(const Vector3d &) = 0;
		virtual void normal(const Vector3f &) = 0;
		virtual void normal(const Vector3d &) = 0;
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
		GLShaderObject so;
		GLhandleARB shader;

	protected:
		explicit GLShader(GLenum shader_type)
		{
			shader = so.glCreateShaderObject(shader_type);
		}

	public:
		~GLShader()
		{
			so.glDeleteObject(shader);
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
		GLShaderObject so;
		GLhandleARB program;

	public:
		GLProgram()
		{
			program = so.glCreateProgramObject();
		}

		~GLProgram()
		{
			so.glDeleteObject(program);
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
}
