#pragma once

#include <vector>
#include <iostream>
#include "GLCaller.h"

using namespace Kino;

class Camera
{
public:
	Camera(const Vector3f &, const Vector3f &, const Vector3f &);
	const Matrix4f getMatrix() const;

public:
	Vector3f pos;
	Vector3f dir;
	Vector3f up;
};

class LightSource
{
public:
	LightSource(const Vector3f &, const Vector3f &, const Color &);
	void process();

public:
	Vector3f pos;
	Vector3f dir;
	Color color;
};

class Object3D
{
public:
	virtual ~Object3D();

	virtual void render() const = 0;
	virtual Vector4d position() const = 0;

public:
	double depth;

};

class Sphere3D : public Object3D
{
public:
	Sphere3D(const Vector3d &, double, const Color &);
	virtual void render() const;
	virtual Vector4d position() const;

private:
	Vector3d pos;
	double radius;
	Color color;
};

class Cylinder3D : public Object3D
{
public:
	Cylinder3D(const Vector3d &, const Vector3d &, double, const Color &);
	virtual void render() const;
	virtual Vector4d position() const;

private:
	Vector3d begin;
	Vector3d end;
	double radius;
	Color color;
};

class Cone3D : public Object3D
{
public:
	Cone3D(const Vector3d &, const Vector3d &, double, const Color &);
	virtual void render() const;
	virtual Vector4d position() const;

private:
	Vector3d begin;
	Vector3d end;
	double radius;
	Color color;
};

class Triangle3D : public Object3D
{
public:
	Triangle3D(const Vector3d &, const Vector3d &, const Vector3d &, const Color &);
	virtual void render() const;
	virtual Vector4d position() const;

private:
	Vector3d a;
	Vector3d b;
	Vector3d c;
	Color color;
};

class Scene3D
{
public:
	Scene3D();
	~Scene3D();

	void render(unsigned, unsigned);
	void addObject(Object3D *);
	void addLightSource(LightSource *);
	void setCamera(Camera *);

protected:
	void drawScene();
	void sortObjects();

private:
	Camera * camera;
	std::vector<Object3D *> objects;
	std::vector<LightSource *> lights;
};
