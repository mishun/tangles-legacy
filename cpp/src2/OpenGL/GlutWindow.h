#pragma once

#include <string>
#include "OpenGL.h"

namespace OpenGL
{
	class GlutWindow
	{
	private:
		size_t width;
		size_t height;
		std::string name;

	public:
		GlutWindow(size_t, size_t, const char *);
		virtual ~GlutWindow();

		void run(int *, char *[]);
		void text(double, double, const Color &, const char *);

	public:
		virtual bool init(OpenGL &) = 0;
		virtual bool draw(OpenGL &) = 0;
		virtual void resize(OpenGL &, size_t, size_t) = 0;

		virtual void keyboardCharacter(unsigned char, double, double);
		virtual void keyboardUp(double, double);
		virtual void keyboardDown(double, double);
		virtual void keyboardLeft(double, double);
		virtual void keyboardRight(double, double);

		virtual void mouseLeft(bool, double, double);
		virtual void mouseRight(bool, double, double);
		virtual void mouseMove(double, double);

	private:
		GlutWindow(const GlutWindow &);
		GlutWindow & operator=(const GlutWindow &);
	};
}
