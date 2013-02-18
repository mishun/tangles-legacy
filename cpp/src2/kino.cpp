#include <cmath>
#include <ctime>
#include <vector>
#include <string>
#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include "Kino/graph.h"
#include <OpenGL/GlutWindow.h>

extern GLCaller gl;
extern GL::GLProgram * gl_program;
extern GL::GLTexture * shadow_map;
extern GL::GLFrameBufferObject * gl_framebuffer;


size_t shadow_map_size = 512;

const OpenGL::Color background_color(1.0f, 1.0f, 1.0f);
const Color hot_color(0.0f, 0.0f, 1.0f/*, 0.7f*/);
const Color cold_color(0.0f, 0.0f, 1.0f/*, 0.7f*/);
const Color stick_color(1.0f, 0.0f, 0.0f/*, 0.6f*/);
const Color axis_color(0.7f, 0.7f, 0.7f, 0.5f);

const double axis_length = 20;
const double initial_distance = 40.0;
const double graphite_dist = 1.52;
const double diamond_dist = 2.35;


const double d_r = 1.0;
const double d_phi = 0.004;
const double d_theta = 0.004;
double phi = 0, theta = Geometry::pi / 2, r = initial_distance;
double v_phi = 0;
bool show_diamond = true;
bool show_graphite = true;
bool show_axis = false;


unsigned window_width;
unsigned window_height;

class KinoState
{
private:
	struct Frame
	{
	public:
		Frame()
		{
		}

		Frame(const std::vector<Vector3d> & _p, const std::vector<double> & _ke)
			: p(_p), ke(_ke)
		{
		}
	public:
		std::vector<Vector3d> p;
		std::vector<double> ke;
	};

public:
	KinoState()
		: play(true)
		, current_frame(0)
		, steps(16)
	{
	}

	unsigned lastFrame() const
	{
		return (frames.size() - 1) * steps;
	}

	unsigned currentFrame() const
	{
		return current_frame;
	}

	size_t numberOfFrames() const
	{
		return (frames.size() - 1) * steps + 1;
	}

	void read(const std::string &);
	Vector3d getCenter(const std::vector<Vector3d> &) const;
	void draw(unsigned, OpenGL::GlutWindow &) const;

public:
	bool play;
	unsigned current_frame;

private:
	const unsigned steps;
	std::vector<Frame> frames;
};

void KinoState::read(const std::string & file)
{
	double time = clock();

	std::ifstream in(file.c_str());
	if(!in)
		throw std::runtime_error("can not open file");

	std::vector<Vector3d> curp;
	std::vector<double> curke;
	bool in_shit = true;

	std::string line;
	while(!in.eof())
	{
		getline(in, line);
		if(line.empty())
			continue;

		unsigned id;
		char atom;
		double x, y, z;
		double vx, vy, vz;
		double ke, pe, te;
		int f1, f2;
		if(sscanf(line.c_str(), "%u %c %lf %lf %lf %lf %lf %lf %lf %lf %lf %i %i\n", &id, &atom, &x, &y, &z, &vx, &vy, &vz, &ke, &pe, &te, &f1, &f2) != 13)
		{
			if(!in_shit)
			{
				frames.push_back(Frame(curp, curke));
				curp.clear();
				curke.clear();
			}
			in_shit = true;
			continue;
		}

		in_shit = false;
		curp.push_back(Vector3d(x, y, z));
		curke.push_back(ke);
	}

	if(!in_shit)
		frames.push_back(Frame(curp, curke));

	if(frames.size() == 0)
		throw std::runtime_error("bad file");

	for(unsigned i = 0; i + 1 < frames.size(); i++)
		if(frames[i].p.size() != frames[i + 1].p.size())
			throw std::runtime_error("bad file");

	time = (clock() - time) / CLOCKS_PER_SEC;
	std::cerr << "reading: " << time << "s\n";
}

Vector3d KinoState::getCenter(const std::vector<Vector3d> & p) const
{
	const double max_dist = 20.0;

	Vector3d center(0, 0, 0);
	double cnt = 0;
	for(unsigned i = 0; i < p.size(); i++)
		if(fabs(p[i].x) < max_dist && fabs(p[i].y) < max_dist && fabs(p[i].z) < max_dist)
		{
			center += p[i];
			cnt++;
		}

	return center * (1.0 / cnt);
}

void KinoState::draw(unsigned index, OpenGL::GlutWindow & window) const
{
	const double hot_ke = 0.02;

	unsigned frame = index / steps;
	unsigned step = index % steps;

	std::vector<Vector3d> positions(frames[frame].p.size());
	std::vector<double> temp(frames[frame].p.size());
	if(frame + 1 < frames.size())
	{
		double p = (double)(steps - step) / (double)steps;
		double q = 1.0 - p;

		for(unsigned i = 0; i < positions.size(); i++)
		{
			positions[i] = frames[frame].p[i] * p + frames[frame + 1].p[i] * q;

			double a0 = std::min(1.0, frames[frame].ke[i] / hot_ke);
			double a1 = std::min(1.0, frames[frame + 1].ke[i] / hot_ke);
			temp[i] = a0 * p + a1 * q;
		}
	}
	else
	{
		positions = frames[frame].p;
		for(unsigned i = 0; i < positions.size(); i++)
		{
			temp[i] = std::min(1.0, frames[frame].ke[i] / hot_ke);
		}
	}

	Vector3d center = getCenter(positions);

	Scene3D scene;

	Vector3f dir(cos(phi) * sin(theta), sin(phi) * sin(theta), cos(theta));
	Vector3f up(-cos(phi) * cos(theta), -sin(phi) * cos(theta), sin(theta));
	phi += v_phi;

	{
		Vector3f centerf(center.x, center.y, center.z);
		scene.setCamera(new Camera(dir * r + centerf, centerf, up));
		scene.addLightSource(new LightSource(centerf + Vector3f(30, 0, 0), centerf, Color(1.0, 1.0, 1.0)));
	}

	const double threshold = 3.0;
	for(unsigned i = 0; i < positions.size(); i++)
	{
		Color color = Color::interpolate(hot_color, cold_color, temp[i]);

		//if((positions[i] - center).length() < threshold)
		//	color = Color::Green;

		double r = 0.3;
		if(i >= 70 && i < 100)
		{
			color = Color::Green;
			r = 0.5;
		}

		scene.addObject(new Sphere3D(positions[i], r, color));
	}

	for(unsigned i = 0; i < positions.size(); i++)
		for(unsigned j = 0; j < positions.size(); j++)
		{
			double dist_sqr = (positions[i] - positions[j]).squaredLength();
			if(dist_sqr <= diamond_dist * diamond_dist)
			{
				//Color color(0.5, 0.5, 0.5, 0.6);
				Color color = stick_color;
				if((positions[i] - center).length() < threshold || (positions[j] - center).length() < threshold)
					color = Color::Green;

				scene.addObject(new Cylinder3D(positions[i], positions[j], 0.1, color));
			}
		}

/*	for(unsigned i = 0; i < positions.size(); i++)
		for(unsigned j = 0; j < i; j++)
			for(unsigned k = 0; k < positions.size(); k++)
			{
				if(k == i || k == j)
					continue;

				double d1 = (positions[i] - positions[k]).abs();
				double d2 = (positions[j] - positions[k]).abs();

				if(d1 < 1.66 && d2 < 1.66)
				{
					scene.addObject(new Triangle3D(positions[i], positions[j], positions[k], Color(0, 1, 0, 0.5)));
				}
			}*/

	if(show_axis)
	{
		Vector3d dx(1.0, 0.0, 0.0), dy(0.0, 1.0, 0.0), dz(0.0, 0.0, 1.0);
		scene.addObject(new Cylinder3D(center, center + dx * axis_length, 0.1, axis_color));
		scene.addObject(new Cylinder3D(center, center + dy * axis_length, 0.1, axis_color));
		scene.addObject(new Cylinder3D(center, center + dz * axis_length, 0.1, axis_color));
		scene.addObject(new Cone3D(center + dx * axis_length, center + dx * (axis_length + 2.0), 1.0, axis_color));
		scene.addObject(new Cone3D(center + dy * axis_length, center + dy * (axis_length + 2.0), 1.0, axis_color));
		scene.addObject(new Cone3D(center + dz * axis_length, center + dz * (axis_length + 2.0), 1.0, axis_color));
	}

	scene.render(window_width, window_height);
}

KinoState kino;


class KinoWindow : public OpenGL::GlutWindow
{
protected:
	bool mouse_active;
	double mouse_x;
	double mouse_y;
	unsigned myframe;
	double mytime;
	double fps;

public:
	KinoWindow()
		: GlutWindow(640, 480, "Kino")
		, mouse_active(false)
		, mouse_x(0)
		, mouse_y(0)
		, myframe(0)
		, mytime(0)
		, fps(0)
	{
	}

	virtual bool init(OpenGL::OpenGL &);
	virtual bool draw(OpenGL::OpenGL &);
	virtual void resize(OpenGL::OpenGL &, size_t, size_t);
	virtual void keyboardCharacter(unsigned char, double, double);
	virtual void keyboardUp(double, double);
	virtual void keyboardDown(double, double);
	virtual void keyboardLeft(double, double);
	virtual void keyboardRight(double, double);
	virtual void mouseLeft(bool, double, double);
	virtual void mouseMove(double, double);
};

bool KinoWindow::init(OpenGL::OpenGL & gl)
{
	::gl.loadExtensions();

	gl.enableDepthTest();
	gl.enableBlend();

	gl.clearDepth(1.0f);
	gl.clearColor(background_color);

	glEnable(GL_COLOR_MATERIAL);
	float mat_specular[] = {1.0f, 1.0f, 1.0f, 0.0f};
	glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
	glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, mat_specular);
	glMaterialf(GL_FRONT, GL_SHININESS, 128.0);


	GL::GLVertexShader & vertex_shader = *new GL::GLVertexShader();
	vertex_shader.shaderFile("../shaders/kino.vert");
	vertex_shader.compileShader();
	std::cout << "vertex shader compiled = " << vertex_shader.isCompiled() << "\n";
	std::cout << vertex_shader.getInfoLog() << "\n";

	GL::GLFragmentShader & fragment_shader = *new GL::GLFragmentShader();
	fragment_shader.shaderFile("../shaders/kino.frag");
	fragment_shader.compileShader();
	std::cout << "fragment shader compiled = " << fragment_shader.isCompiled() << "\n";
	std::cout << fragment_shader.getInfoLog() << "\n";

	GL::GLProgram & program = *new GL::GLProgram();
	program.attachObject(vertex_shader);
	program.attachObject(fragment_shader);
	program.linkProgram();
	std::cout << "program linked = " << program.isLinked() << "\n";
	std::cout << program.getInfoLog() << "\n";

	gl_program = &program;



	GL::GLTexture & shadow_map = *new GL::GLTexture();
	glBindTexture(GL_TEXTURE_2D, shadow_map.get());
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, shadow_map_size, shadow_map_size, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE_ARB); //Enable shadow comparison
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC_ARB, GL_LEQUAL); //Shadow comparison should be true (ie not in shadow) if r<=texture
	glTexParameteri(GL_TEXTURE_2D, GL_DEPTH_TEXTURE_MODE_ARB, GL_INTENSITY); //Shadow comparison should generate an INTENSITY result

	::shadow_map = &shadow_map;


	GL::GLFrameBufferObject & framebuffer = *new GL::GLFrameBufferObject();
	framebuffer.bind();
	GLCaller::gl().fbo().glFramebufferTexture2D(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, shadow_map.get(), 0);
	glDrawBuffer(GL_NONE);
	glReadBuffer(GL_NONE);
	::gl.fbo().glBindFramebuffer(GL_FRAMEBUFFER_EXT, 0);

	gl_framebuffer = &framebuffer;

	return true;
}

bool KinoWindow::draw(OpenGL::OpenGL & gl)
{
	kino.draw(kino.current_frame, *this);
	if(kino.play && kino.current_frame < kino.lastFrame())
		kino.current_frame++;

	{
		if(myframe == 0)
		{
			fps = 12.0 * CLOCKS_PER_SEC / (clock() - mytime);
			mytime = clock();
			myframe = 12;
		}
		myframe--;

		std::ostringstream out;
		out << "Frame: " << kino.currentFrame() + 1 << "/" << kino.numberOfFrames() << " FPS = " << fps;
		text(10, 10, OpenGL::Color(0, 0, 0, 0.7), out.str().c_str());
	}

	return true;
}

void KinoWindow::resize(OpenGL::OpenGL & gl, size_t width, size_t height)
{
	window_width = (unsigned)width;
	window_height = (unsigned)height;
}

void KinoWindow::keyboardCharacter(unsigned char key, double x, double y)
{
	switch(key)
	{
	case 27:
		exit(0);
		break;

	case 'r':
		kino.current_frame = 0;
		break;

	case 'e':
		kino.current_frame = kino.lastFrame();
		break;

	case 'x':
		show_graphite = !show_graphite;
		break;

	case 'z':
		show_diamond = !show_diamond;
		break;

	case 'o':
		show_axis = !show_axis;
		break;

	case ' ':
		kino.play = !kino.play;
		break;
	}
}

void KinoWindow::keyboardUp(double, double)
{
	r -= d_r;
	if(r < 1)
		r = 1;
}

void KinoWindow::keyboardDown(double, double)
{
	r += d_r;
}

void KinoWindow::keyboardLeft(double, double)
{
	kino.play = false;
	if(kino.current_frame > 0)
		kino.current_frame--;
}

void KinoWindow::keyboardRight(double, double)
{
	kino.play = false;
	if(kino.current_frame < kino.lastFrame())
		kino.current_frame++;
}

void KinoWindow::mouseLeft(bool down, double x, double y)
{
	if(down)
	{
		mouse_active = true;
		mouse_x = x;
		mouse_y = y;
		v_phi = 0;
	}
	else
		mouse_active = false;
}

void KinoWindow::mouseMove(double x, double y)
{
	if(mouse_active)
	{
		int dx = x - mouse_x;
		int dy = y - mouse_y;
		mouse_x = x;
		mouse_y = y;
		phi -= d_phi * dx;
		v_phi = -dx * 0.001;

		theta -= d_theta * dy;
		if(theta > Geometry::pi)
			theta = Geometry::pi;
		if(theta < 0)
			theta = 0;
	}
}

int main(int argc, char * argv[])
{
	std::string filename = "MolDyn.OUT";
	if(argc == 2)
		filename = argv[1];

	try
	{
		kino.read(filename);
	}
	catch(std::runtime_error e)
	{
		std::cerr << "Failed: " << e.what() << '\n';
		return 0;
	}

	KinoWindow window;
	window.run(&argc, argv);
	return 0;
}
