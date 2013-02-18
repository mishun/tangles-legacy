#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include "OpenGL.h"
#include <windows.h>
#include <wingdi.h>

using namespace OpenGL;


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


void GLShader::shaderSource(const std::string & source)
{
	const char * src = source.c_str();
	const GLint len = source.length();
	so.glShaderSource(shader, 1, &src, &len);
}

void GLShader::shaderFile(const std::string & filename)
{
	std::ifstream in(filename.c_str());
	std::ostringstream shader;

	if(in.good())
	{
		while(!in.eof())
		{
			std::string line;
			getline(in, line);
			shader << line << "\n";
		}
	}

	shaderSource(shader.str());
}

void GLShader::compileShader()
{
	so.glCompileShader(shader);
}

bool GLShader::isCompiled() const
{
	GLint compiled = 0;
	so.glGetObjectParameteriv(shader, GL_OBJECT_COMPILE_STATUS_ARB, &compiled);
	return compiled != 0;
}

const std::string GLShader::getInfoLog() const
{
	GLint log_length = 0;
	so.glGetObjectParameteriv(shader, GL_OBJECT_INFO_LOG_LENGTH_ARB, &log_length);
	std::vector<char> log(log_length);
	GLint laux;
	so.glGetInfoLog(shader, log_length, &laux, &log[0]);
	return std::string(&log[0]);
}


void GLProgram::attachObject(const GLShader & shader)
{
	so.glAttachObject(program, shader.get());
}

void GLProgram::detachObject(const GLShader & shader)
{
	so.glDetachObject(program, shader.get());
}

void GLProgram::linkProgram()
{
	so.glLinkProgram(program);
}

bool GLProgram::isLinked() const
{
	GLint linked = 0;
	so.glGetObjectParameteriv(program, GL_OBJECT_LINK_STATUS_ARB, &linked);
	return linked != 0;
}

const std::string GLProgram::getInfoLog() const
{
	GLint log_length = 0;
	so.glGetObjectParameteriv(program, GL_OBJECT_INFO_LOG_LENGTH_ARB, &log_length);
	std::vector<char> log(log_length);
	GLint laux;
	so.glGetInfoLog(program, log_length, &laux, &log[0]);
	return std::string(&log[0]);
}

void GLProgram::use() const
{
	so.glUseProgramObject(program);
}

GLint GLProgram::getUniformLocation(const char * name) const
{
	std::cerr << "d = " << so.glGetUniformLocation << "\n";
	return so.glGetUniformLocation(program, name);
}
