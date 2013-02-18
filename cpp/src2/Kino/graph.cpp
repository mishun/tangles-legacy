#include <iostream>
#include <algorithm>
#include "graph.h"
#include <GL/glut.h>


GLCaller gl;
GL::GLProgram * gl_program;
GL::GLTexture * shadow_map;
GL::GLFrameBufferObject * gl_framebuffer;

extern size_t shadow_map_size;


Camera::Camera(const Vector3f & _pos, const Vector3f & _dir, const Vector3f & _up)
	: pos(_pos), dir(_dir), up(_up)
{
}

const Matrix4f Camera::getMatrix() const
{
	return Geometry::lookAtMatrix(pos, dir - pos, up);
}


Object3D::~Object3D()
{
}


LightSource::LightSource(const Vector3f & _pos, const Vector3f & _dir, const Color & _color)
	: pos(_pos)
	, dir(_dir)
	, color(_color)
{
}

void LightSource::process()
{
	glEnable(GL_LIGHT0);
	glLightfv(GL_LIGHT0, GL_POSITION, Vector4f(pos, 1.0f).data());
	//glLightfv(GL_LIGHT0, GL_AMBIENT, color.data());
	glLightfv(GL_LIGHT0, GL_DIFFUSE, color.data());
	glLightfv(GL_LIGHT0, GL_SPECULAR, color.data());
}


Scene3D::Scene3D()
	: camera(0)
{
}

Scene3D::~Scene3D()
{
	delete camera;

	for(unsigned i = 0; i < objects.size(); i++)
		delete objects[i];

	for(unsigned i = 0; i < lights.size(); i++)
		delete lights[i];
}

void Scene3D::render(unsigned width, unsigned height)
{
	if(camera == 0)
		return;

	Matrix4f cameraProjectionMatrix = Geometry::perspectiveMatrix(45.0f, (float)width / (float)height, 1.0f, 50.0f);
	Matrix4f cameraViewMatrix = camera->getMatrix();

	glEnable(GL_CULL_FACE);
	for(unsigned light_index = 0; light_index < lights.size(); light_index++)
	{
		LightSource & light = *lights[light_index];

		Matrix4f lightProjectionMatrix = Geometry::perspectiveMatrix(35.0f, 1.0f, 2.0f, 64.0f);

		Vector3f delta = light.dir - light.pos;
		Vector3f up = (fabs(delta.y) > fabs(delta.z)) ? Vector3f(-delta.y, delta.x, 0.0).normalized() : Vector3f(-delta.z, 0.0, delta.x).normalized();
		Matrix4f lightViewMatrix = Geometry::lookAtMatrix(light.pos, light.dir - light.pos, up);


		gl_framebuffer->bind();
		gl.so().glUseProgramObject(0);

		gl.matrix.setProjection(lightProjectionMatrix);
		gl.matrix.setModelView(lightViewMatrix);

		glViewport(0, 0, shadow_map_size, shadow_map_size);
		glColorMask(0, 0, 0, 0);
		glCullFace(GL_FRONT);
		glShadeModel(GL_FLAT);

		glClear(GL_DEPTH_BUFFER_BIT);
		drawScene();



		gl.fbo().glBindFramebuffer(GL_FRAMEBUFFER_EXT, 0);
		gl.so().glUseProgramObject(gl_program->get());

		gl.matrix.setProjection(cameraProjectionMatrix);
		gl.matrix.setModelView(cameraViewMatrix);

		Matrix4f biasMatrix = Geometry::scaleMatrix(0.5f) * Geometry::translationMatrix(Vector3f(1.0f, 1.0f, 1.0f)); //bias from [-1, 1] to [0, 1]
		Matrix4f shadow_matrix = biasMatrix * lightProjectionMatrix * lightViewMatrix;

		gl.so().glUniform4fv(gl_program->getUniformLocation("light_to_camera_position"), 1, (cameraViewMatrix * Vector4f(light.pos, 1.0f)).data());
		gl.so().glUniformMatrix4fv(gl_program->getUniformLocation("shadow_matrix"), 1, false, (shadow_matrix * cameraViewMatrix.inverse()).data());
		gl.so().glUniform1i(gl_program->getUniformLocation("sampler"), 0);

		sortObjects();

		glViewport(0, 0, width, height);
		glColorMask(1, 1, 1, 1);
		glCullFace(GL_BACK);

		light.process();

		glBindTexture(GL_TEXTURE_2D, shadow_map->get());

		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		drawScene();

		glDisable(GL_LIGHT0);
	}
}

void Scene3D::addObject(Object3D * object)
{
	objects.push_back(object);
}

void Scene3D::addLightSource(LightSource * light)
{
	lights.push_back(light);
}

void Scene3D::setCamera(Camera * _camera)
{
	camera = _camera;
}

void Scene3D::drawScene()
{
	for(unsigned i = 0; i < objects.size(); i++)
		objects[i]->render();
}

void Scene3D::sortObjects()
{
	Matrix4d depth = gl.matrix.getProjection() * gl.matrix.getModelView();
	for(unsigned i = 0; i < objects.size(); i++)
	{
		objects[i]->depth = (depth * objects[i]->position()).z;
	}

	std::sort(objects.begin(), objects.end(), [](Object3D * a, Object3D * b) -> bool { return a->depth > b->depth; });
}


Sphere3D::Sphere3D(const Vector3d & _pos, double _radius, const Color & _color)
	: pos(_pos)
	, radius(_radius)
	, color(_color)
{
}

void Sphere3D::render() const
{
	static Kino::VertexBufferObject sphere;
	static std::vector<Vector3f> vbo;

	if(sphere.null())
	{
		static const double pi = acos(-1.0);
		static const unsigned lon_points = 16, lat_points = 8;

		for(unsigned loni = 0; loni < lon_points; loni++)
		{
			double lon1 = 2.0 * pi * loni / lon_points;
			double lon2 = 2.0 * pi * (loni + 1) / lon_points;

			for(unsigned lati = 0; lati <= lat_points; lati++)
			{
				double lat1 = pi * lati / (lat_points + 1);
				double lat2 = pi * (lati + 1) / (lat_points + 1);

				if(lati == 0)
				{
					vbo.push_back(Vector3f(0.0f, 0.0f, 1.0f));
					vbo.push_back(Vector3f(cos(lon1) * sin(lat2), sin(lon1) * sin(lat2), cos(lat2)));
					vbo.push_back(Vector3f(cos(lon2) * sin(lat2), sin(lon2) * sin(lat2), cos(lat2)));
				}
				else if(lati == lat_points)
				{
					vbo.push_back(Vector3f(cos(lon2) * sin(lat1), sin(lon2) * sin(lat1), cos(lat1)));
					vbo.push_back(Vector3f(cos(lon1) * sin(lat1), sin(lon1) * sin(lat1), cos(lat1)));
					vbo.push_back(Vector3f(0.0f, 0.0f, -1.0f));
				}
				else
				{
					vbo.push_back(Vector3f(cos(lon2) * sin(lat1), sin(lon2) * sin(lat1), cos(lat1)));
					vbo.push_back(Vector3f(cos(lon1) * sin(lat1), sin(lon1) * sin(lat1), cos(lat1)));
					vbo.push_back(Vector3f(cos(lon1) * sin(lat2), sin(lon1) * sin(lat2), cos(lat2)));
					vbo.push_back(Vector3f(cos(lon1) * sin(lat2), sin(lon1) * sin(lat2), cos(lat2)));
					vbo.push_back(Vector3f(cos(lon2) * sin(lat2), sin(lon2) * sin(lat2), cos(lat2)));
					vbo.push_back(Vector3f(cos(lon2) * sin(lat1), sin(lon2) * sin(lat1), cos(lat1)));
				}
			}
		}

		sphere = Kino::VertexBufferObject::create();
		sphere.bufferData(&vbo[0], vbo.size() * sizeof(Vector3f));
	}

	gl.matrix.pushModelView();
	gl.matrix.multiplyModelView(Geometry::translationMatrix(pos) * Geometry::scaleMatrix(radius));

	gl.glColor(color);
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	sphere.bind();
	glVertexPointer(3, GL_FLOAT, 0, 0);
	glNormalPointer(GL_FLOAT, 0, 0);
	glDrawArrays(GL_TRIANGLES, 0, vbo.size());

	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);

	gl.matrix.popModelView();
}

Vector4d Sphere3D::position() const
{
	return Vector4d(pos, 1.0);
}


Cylinder3D::Cylinder3D(const Vector3d & _begin, const Vector3d & _end, double _radius, const Color & _color)
	: begin(_begin)
	, end(_end)
	, radius(_radius)
	, color(_color)
{
}

void Cylinder3D::render() const
{
	static Kino::VertexBufferObject cylinder;
	static std::vector<Vector3f> vbo;

	if(cylinder.null())
	{
		const double _2pi = 2.0 * acos(-1.0);
		const unsigned points = 16;

		for(unsigned i = 0; i <= points; i++)
		{
			double a = _2pi * i / points;
			vbo.push_back(Vector3f(cos(a), sin(a), 1.0f));
			vbo.push_back(Vector3f(cos(a), sin(a), 0.0f));
			vbo.push_back(Vector3f(cos(a), sin(a), 0.0f));
			vbo.push_back(Vector3f(cos(a), sin(a), 0.0f));
		}

		cylinder = Kino::VertexBufferObject::create();
		cylinder.bufferData(&vbo[0], vbo.size() * sizeof(Vector3f));
	}

	gl.matrix.pushModelView();
	gl.matrix.multiplyModelView(Geometry::cylinderMatrix(begin, end, radius));

	gl.glColor(color);

	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);

	cylinder.bind();
	glVertexPointer(3, GL_FLOAT, 2 * sizeof(Vector3f), 0);
	glNormalPointer(GL_FLOAT, 2 * sizeof(Vector3f), (void *)sizeof(Vector3f));

	glDrawArrays(GL_TRIANGLE_STRIP, 0, vbo.size() / 2);

	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);

	gl.matrix.popModelView();
}

Vector4d Cylinder3D::position() const
{
	return Vector4d((begin + end) * 0.5, 1.0);
}


Cone3D::Cone3D(const Vector3d & _begin, const Vector3d & _end, double _radius, const Color & _color)
	: begin(_begin)
	, end(_end)
	, radius(_radius)
	, color(_color)
{
}

void Cone3D::render() const
{
	static Kino::VertexBufferObject cone;
	static std::vector<Vector3f> vbo;

	if(cone.null())
	{
		const double _2pi = 2.0 * acos(-1.0);
		const unsigned points = 32;

		for(unsigned i = 0; i < points; i++)
		{
			double a = _2pi * i / points;
			double b = _2pi * (i + 1) / points;

			vbo.push_back(Vector3f(0.0f, 0.0f, 1.0f));
			vbo.push_back(Vector3f(cos(0.5 * (a + b)), sin(0.5 * (a + b)), 1.0).normalized());
			vbo.push_back(Vector3f(cos(a), sin(a), 0.0f));
			vbo.push_back(Vector3f(cos(a), sin(a), 1.0).normalized());
			vbo.push_back(Vector3f(cos(b), sin(b), 0.0f));
			vbo.push_back(Vector3f(cos(b), sin(b), 1.0).normalized());
		}

		cone = Kino::VertexBufferObject::create();
		cone.bufferData(&vbo[0], vbo.size() * sizeof(Vector3f));
	}

	gl.matrix.pushModelView();
	gl.matrix.multiplyModelView(Geometry::cylinderMatrix(begin, end, radius));

	gl.glColor(color);

	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);

	cone.bind();
	glVertexPointer(3, GL_FLOAT, 2 * sizeof(Vector3f), 0);
	glNormalPointer(GL_FLOAT, 2 * sizeof(Vector3f), (void *)sizeof(Vector3f));

	glDrawArrays(GL_TRIANGLES, 0, vbo.size() / 2);

	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);

	gl.matrix.popModelView();
}

Vector4d Cone3D::position() const
{
	return Vector4d((begin + end) * 0.5, 1.0);
}


Triangle3D::Triangle3D(const Vector3d & _a, const Vector3d & _b, const Vector3d & _c, const Color & _color)
	: a(_a)
	, b(_b)
	, c(_c)
	, color(_color)
{
}

void Triangle3D::render() const
{
	Vector3d norm = ((b - a) ^ (c - a)).normalized();

	gl.glColor(color);
	gl.glBegin(GL_TRIANGLES);
	gl.glNormal(norm);
	gl.glVertex(a);
	gl.glVertex(b);
	gl.glVertex(c);
	gl.glEnd();
}

Vector4d Triangle3D::position() const
{
	return Vector4d((a + b + c) * (1.0 / 3.0), 1.0);
}
