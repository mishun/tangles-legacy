#include <cassert>
#include <iostream>
#include <OpenGL/OpenGL.h>
#include <Topology/EmbeddedGraph.h>
#include <Util/Util.h>
#include <Geometry/Geometry.h>
#include <OpenGL/GlutWindow.h>

using Topology::EmbeddedGraph;
using Geometry::Vector3D;

OpenGL::GLProgram * gl_program;
OpenGL::GLShaderObject * so;


class SphereCameraPosition
{
protected:
	double radius;
	double theta;
	double phi;

public:
	SphereCameraPosition(double);

	OpenGL::Matrix4f getViewMatrix() const;
	void shift(double, double, double);
};

SphereCameraPosition::SphereCameraPosition(double r)
	: radius(r)
	, theta(Geometry::pi / 2.0)
	, phi(0)
{
}

OpenGL::Matrix4f SphereCameraPosition::getViewMatrix() const
{
	OpenGL::Vector3f dir(cos(phi) * sin(theta), sin(phi) * sin(theta), cos(theta));
	OpenGL::Vector3f up(-cos(phi) * cos(theta), -sin(phi) * cos(theta), sin(theta));

	return OpenGL::MatrixUtil::lookAtMatrix(dir * radius, -dir, up);
}

void SphereCameraPosition::shift(double dphi, double dtheta, double dr)
{
	phi += dphi;
	theta += dtheta;
	radius += dr;

	if(theta > Geometry::pi)
		theta = Geometry::pi;

	if(theta < 0)
		theta = 0;
}

SphereCameraPosition cameraPosition(2.0);



class Graph3D
{
protected:
	EmbeddedGraph * graph;
	size_t n;
	std::vector<OpenGL::Vector3d> x;
	std::vector<OpenGL::Vector3d> g;

public:
	Graph3D()
		: graph(0)
	{
	}

	void init(EmbeddedGraph & gr)
	{
		assert(gr.isTriangulation());

		graph = &gr;
		n = graph->numberOfVertexes();

		x.assign(n, OpenGL::Vector3d());
		g.assign(n, OpenGL::Vector3d());

		Util::MersenneTwister mt(69);
		for(size_t i = 0; i < n; i++)
		{
			x[i].x = 2.0 * mt.nextDouble() - 1.0;
			x[i].y = 2.0 * mt.nextDouble() - 1.0;
			x[i].z = 2.0 * mt.nextDouble() - 1.0;
		}

		init();
	}

	void draw(OpenGL::OpenGL & gl)
	{
		if(graph == 0)
			return;

		OpenGL::Matrix4f viewMatrix = cameraPosition.getViewMatrix();
		gl.loadModelViewMatrix(viewMatrix);

		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		so->glUseProgramObject(gl_program->get());
		so->glUniform4fv(so->glGetUniformLocation(gl_program->get(), "light_to_camera_position"), 1, (viewMatrix * OpenGL::Vector4f(30, 0, 0, 1.0)).data());
		//so->glUniformMatrix4fv(gl_program->getUniformLocation("shadow_matrix"), 1, false, (shadow_matrix * cameraViewMatrix.inverse()).data());

		gl.color(OpenGL::Color::Red);
		gl.beginTriangles();

		for(size_t i = 0; i < graph->numberOfFaces(); i++)
		{
			const EmbeddedGraph::Face & f = graph->face(i);

			OpenGL::Vector3d cur =  x[ f.incidentVertex(0).vertex.idInGraph() ];
			OpenGL::Vector3d next = x[ f.incidentVertex(1).vertex.idInGraph() ];
			OpenGL::Vector3d prev = x[ f.incidentVertex(2).vertex.idInGraph() ];

			const OpenGL::Vector3d norm = ((prev - cur) ^ (next - cur)).normalized();

			gl.normal(norm);

			for(size_t j = 0; j < f.numberOfIncidentVertexes(); j++)
			{
				const EmbeddedGraph::Vertex & v = f.incidentVertex(j).vertex;
				OpenGL::Vector3d p = x[v.idInGraph()];

				gl.vertex(p);
			}
		}

		gl.end();

		so->glUseProgramObject(0);

		gl.color(OpenGL::Color::Green);
		gl.beginLines();

		for(size_t i = 0; i < n; i++)
		{
			OpenGL::Vector3d a = x[i];
			OpenGL::Vector3d b = x[i] + g[i];

			gl.vertex(a);
			gl.vertex(b);
		}

		gl.end();


		step();
	}

protected:
	void getForce()
	{
		for(size_t i = 0; i < n; i++)
			g[i] = OpenGL::Vector3d(0, 0, 0);

		const double k = 0.01;

		for(size_t i = 0; i < graph->numberOfEdges(); i++)
		{
			const EmbeddedGraph::Edge & e = graph->edge(i);

			size_t ai = e.begin().vertex.idInGraph();
			size_t bi = e.end().vertex.idInGraph();

			OpenGL::Vector3d a = x[ai];
			OpenGL::Vector3d b = x[bi];

			//double d = (b - a).squaredLength();

			OpenGL::Vector3d dir = (b - a).normalized();
			double len = exp((b - a).length());

			g[ai] = g[ai] + dir * len * k;
			g[bi] = g[bi] - dir * len * k;
		}


		const double l = 0.03;

		for(size_t i = 0; i < graph->numberOfFaces(); i++)
		{
			const EmbeddedGraph::Face & f = graph->face(i);

			size_t ai = f.incidentVertex(0).vertex.idInGraph();
			size_t bi = f.incidentVertex(1).vertex.idInGraph();
			size_t ci = f.incidentVertex(2).vertex.idInGraph();

			OpenGL::Vector3d a = x[ai];
			OpenGL::Vector3d b = x[bi];
			OpenGL::Vector3d c = x[ci];

			OpenGL::Vector3d norm = ((c - a) ^ (b - a)).normalized();

			g[ai] = g[ai] + norm * l;
			g[bi] = g[bi] + norm * l;
			g[ci] = g[ci] + norm * l;
		}
	}

	void init()
	{
		//relax();
		getForce();
	}

	void step()
	{
		static size_t iteration = 0;
		std::cerr << "iteration " << iteration << "\n";
		iteration++;

		double speed = 0.2;

		for(size_t i = 0; i < n; i++)
			x[i] = x[i] + g[i] * speed;

		//relax();
		getForce();
	}

	void relax()
	{
		double max_dist = 0.0;

		for(size_t i = 0; i < n; i++)
			for(size_t j = 0; j < i; j++)
				max_dist = std::max(max_dist, (x[i] - x[j]).length());

		for(size_t i = 0; i < n; i++)
			x[i] = x[i] * (1.0 / max_dist);
	}

	/*double volume(const std::vector<Vector3D> & x) const
	{
		double volume = 0.0;

		for(size_t i = 0; i < graph->numberOfFaces(); i++)
		{
			const EmbeddedGraph::Face & f = graph->face(i);

			Vector3D cur = x[ f.incidentVertex(0).vertex.idInGraph() ];
			Vector3D next = x[ f.incidentVertex(1).vertex.idInGraph() ];
			Vector3D prev = x[ f.incidentVertex(2).vertex.idInGraph() ];

			volume += ((prev - cur) ^ (next - cur)) * cur;
		}

		return volume / 6;
	}*/
};

Graph3D graph;

void initGraph()
{
	std::vector< std::pair<size_t, size_t> > v;

	v.push_back(std::make_pair(0, 1));
	v.push_back(std::make_pair(0, 0));
	v.push_back(std::make_pair(0, 3));
	v.push_back(std::make_pair(0, 2));

	std::vector< std::vector< std::pair<size_t, size_t> > > g;
	g.push_back(v);


	/*std::vector< std::pair<size_t, size_t> > v;

	v.push_back(std::make_pair(0, 2));
	v.push_back(std::make_pair(0, 3));
	v.push_back(std::make_pair(0, 0));
	v.push_back(std::make_pair(0, 1));

	std::vector< std::vector< std::pair<size_t, size_t> > > g;
	g.push_back(v);*/


	/*std::vector< std::pair<size_t, size_t> > v;

	v.push_back(std::make_pair(0, 3));
	v.push_back(std::make_pair(0, 4));
	v.push_back(std::make_pair(0, 5));
	v.push_back(std::make_pair(0, 0));
	v.push_back(std::make_pair(0, 1));
	v.push_back(std::make_pair(0, 2));

	std::vector< std::vector< std::pair<size_t, size_t> > > g;
	g.push_back(v);*/


	EmbeddedGraph & eg = EmbeddedGraph::createFromVertexAdjacencyList(g);
	EmbeddedGraph & der = eg.derivableGraph();
	EmbeddedGraph & der2 = der.derivableGraph();
	EmbeddedGraph & der3 = der2.derivableGraph();
	EmbeddedGraph & der4 = der3.derivableGraph();
	//EmbeddedGraph & der5 = der4.derivableGraph();

	graph.init(der4);
}

class Window : public OpenGL::GlutWindow
{
protected:
	bool mouse_active;
	double mouse_x;
	double mouse_y;

public:
	Window()
		: GlutWindow(640, 480, "Surface")
		, mouse_active(false)
		, mouse_x(0.0)
		, mouse_y(0.0)
	{
	}

	virtual bool init(OpenGL::OpenGL &);
	virtual bool draw(OpenGL::OpenGL &);
	virtual void resize(OpenGL::OpenGL &, size_t, size_t);
	virtual void keyboardCharacter(unsigned char, double, double);
	virtual void keyboardUp(double, double);
	virtual void keyboardDown(double, double);
	virtual void mouseLeft(bool, double, double);
	virtual void mouseMove(double, double);
};

bool Window::init(OpenGL::OpenGL & gl)
{
	gl.clearDepth(1.0f);
	gl.clearColor(OpenGL::Color::Black);

	gl.enableDepthTest();
	gl.enableBlend();

	glEnable(GL_COLOR_MATERIAL);
	float mat_specular[] = {1.0f, 1.0f, 1.0f, 0.0f};
	glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
	glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, mat_specular);
	glMaterialf(GL_FRONT, GL_SHININESS, 128.0);


	OpenGL::GLVertexShader vertex_shader;
	vertex_shader.shaderFile("../shaders/draw3D.vert");
	vertex_shader.compileShader();
	std::cout << "vertex shader compiled = " << vertex_shader.isCompiled() << "\n";
	std::cout << vertex_shader.getInfoLog() << "\n";

	OpenGL::GLFragmentShader fragment_shader;
	fragment_shader.shaderFile("../shaders/draw3D.frag");
	fragment_shader.compileShader();
	std::cout << "fragment shader compiled = " << fragment_shader.isCompiled() << "\n";
	std::cout << fragment_shader.getInfoLog() << "\n";

	OpenGL::GLProgram * program = new OpenGL::GLProgram();
	program->attachObject(vertex_shader);
	program->attachObject(fragment_shader);
	program->linkProgram();
	std::cout << "program linked = " << program->isLinked() << "\n";
	std::cout << program->getInfoLog() << "\n";

	gl_program = program;
	so = new OpenGL::GLShaderObject();

	if(!vertex_shader.isCompiled() || !fragment_shader.isCompiled() || !program->isLinked())
		return false;

	return true;
}

bool Window::draw(OpenGL::OpenGL & gl)
{
	graph.draw(gl);
	text(10, 10, OpenGL::Color::White, "Hello");
	return true;
}

void Window::resize(OpenGL::OpenGL & gl, size_t width, size_t height)
{
	gl.viewport(0, 0, width, height);
	OpenGL::Matrix4f projectionMatrix = OpenGL::MatrixUtil::perspectiveMatrix(35.0f, 1.0f, 2.0f, 64.0f);
	gl.loadProjectionMatrix(projectionMatrix);
}

void Window::keyboardCharacter(unsigned char key, double x, double y)
{
	switch(key)
	{
	case 27:
		exit(0);
		break;
	}
}

void Window::keyboardUp(double, double)
{
	cameraPosition.shift(0.0, 0.0, -0.25);
}

void Window::keyboardDown(double, double)
{
	cameraPosition.shift(0.0, 0.0, 0.25);
}

void Window::mouseLeft(bool down, double x, double y)
{
	if(down)
	{
		mouse_active = true;
		mouse_x = x;
		mouse_y = y;
		//v_phi = 0;
	}
	else
		mouse_active = false;
}

void Window::mouseMove(double x, double y)
{
	if(mouse_active)
	{
		double dx = x - mouse_x;
		double dy = y - mouse_y;
		mouse_x = x;
		mouse_y = y;
		//v_phi = -dx * 0.001;

		cameraPosition.shift(-0.004 * dx, -0.004 * dy, 0.0);
	}
}

int main(int argc, char * argv[])
{
	initGraph();

	Window window;
	window.run(&argc, argv);

	return 0;
}
