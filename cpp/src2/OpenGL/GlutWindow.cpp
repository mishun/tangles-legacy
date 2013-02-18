#include <stdexcept>
#include <GL/gl.h>
#include <GL/glut.h>
#include "GlutWindow.h"
#include "OpenGLImpl.h"

using namespace OpenGL;

static GlutWindow * instance = 0;
static OpenGLImpl * gl = 0;
static bool running = false;
static double windowWidth;
static double windowHeight;

static void staticDraw()
{
	if(instance == 0)
		return;

	instance->draw(*gl);
	glutSwapBuffers();
}

static void staticReshape(int width, int height)
{
	windowWidth = width;
	windowHeight = height;

	if(instance == 0)
		return;

	instance->resize(*gl, (width < 0) ? 0 : (size_t)width, (height < 0) ? 0 : (size_t)height);
}

static void staticKey(unsigned char key, int x, int y)
{
	if(instance == 0)
		return;

	instance->keyboardCharacter(key, x, y);
}

static void staticSpecial(int key, int x, int y)
{
	if(instance == 0)
		return;

	switch(key)
	{
	case GLUT_KEY_UP:
		instance->keyboardUp(x, y);
		break;

	case GLUT_KEY_DOWN:
		instance->keyboardDown(x, y);
		break;

	case GLUT_KEY_LEFT:
		instance->keyboardLeft(x, y);
		break;

	case GLUT_KEY_RIGHT:
		instance->keyboardRight(x, y);
		break;
	}
}

static void staticMouse(int button, int state, int x, int y)
{
	if(instance == 0)
		return;

	bool down;
	switch(state)
	{
	case GLUT_DOWN:
		down = true;
		break;

	case GLUT_UP:
		down = false;
		break;

	default:
		return;
	}

	switch(button)
	{
	case GLUT_LEFT_BUTTON:
		instance->mouseLeft(down, x, y);
		break;

	case GLUT_RIGHT_BUTTON:
		instance->mouseRight(down, x, y);
		break;
	}
}

static void staticMove(int x, int y)
{
	if(instance == 0)
		return;

	instance->mouseMove(x, y);
}

static void staticIdle()
{
	glutPostRedisplay();
}

static void staticVisible(int vis)
{
	if(vis == GLUT_VISIBLE)
		glutIdleFunc(staticIdle);
	else
		glutIdleFunc(0);
}


GlutWindow::GlutWindow(size_t w, size_t h, const char * n)
	: width(w)
	, height(h)
	, name(n)
{
	if(instance != 0)
		throw std::logic_error("GlutWindow is singleton");

	instance = this;
}

GlutWindow::~GlutWindow()
{
	instance = 0;
}

void GlutWindow::run(int * argc, char * argv[])
{
	if(running)
		throw std::logic_error("already runned");
	running = true;

	glutInit(argc, argv);

	glutInitWindowSize(width, height);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH | GLUT_STENCIL);
	glutCreateWindow(name.c_str());

	glutReshapeFunc(staticReshape);
	glutDisplayFunc(staticDraw);
	glutKeyboardFunc(staticKey);
	glutSpecialFunc(staticSpecial);
	glutMouseFunc(staticMouse);
	glutMotionFunc(staticMove);
	glutVisibilityFunc(staticVisible);

	gl = new OpenGLImpl();

	if(init(*gl))
		glutMainLoop();

	running = false;
}

void GlutWindow::text(double x, double y, const Color & color, const char * text)
{
	if(!running)
		return;

	Matrix4d proj = MatrixUtil::orthoMatrix(0.0, (double)windowWidth, 0.0, (double)windowHeight, -1.0, 1.0);

	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity();

	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadMatrixd(proj.data());

	glColor4f(color.r, color.g, color.b, color.a);
	glRasterPos2f(x, y);
	for(size_t i = 0; text[i] != 0; i++)
		glutBitmapCharacter(GLUT_BITMAP_9_BY_15, text[i]);

	glMatrixMode(GL_PROJECTION);
	glPopMatrix();

	glMatrixMode(GL_MODELVIEW);
	glPopMatrix();
}

void GlutWindow::keyboardCharacter(unsigned char, double, double)
{
}

void GlutWindow::keyboardUp(double, double)
{
}

void GlutWindow::keyboardDown(double, double)
{
}

void GlutWindow::keyboardLeft(double, double)
{
}

void GlutWindow::keyboardRight(double, double)
{
}

void GlutWindow::mouseLeft(bool, double, double)
{
}

void GlutWindow::mouseRight(bool, double, double)
{
}

void GlutWindow::mouseMove(double, double)
{
}
