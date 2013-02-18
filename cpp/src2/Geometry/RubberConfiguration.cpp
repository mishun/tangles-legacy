#include <cassert>
#include <cmath>
#include <iostream>
#include <algorithm>
#include "RubberConfiguration.h"

using namespace Geometry;

RubberConfiguration::RubberConfiguration(size_t s)
	: size(s)
	, x(s)
	, y(s)
{
	clear();
}

RubberConfiguration::~RubberConfiguration()
{
}

void RubberConfiguration::clear()
{
	x.clear();
	y.clear();
}

void RubberConfiguration::connect(size_t u, size_t v, double k)
{
	assert(u < size);
	assert(v < size);

	x.addA(u, u, k);
	x.addA(v, v, k);
	x.addA(u, v, -k);
	x.addA(v, u, -k);

	y.addA(u, u, k);
	y.addA(v, v, k);
	y.addA(u, v, -k);
	y.addA(v, u, -k);
}

void RubberConfiguration::connect(size_t u, const Vector2D & p, double k)
{
	assert(u < size);

	x.addA(u, u, k);
	x.addB(u, p.x);

	y.addA(u, u, k);
	y.addB(u, p.y);
}

std::vector<Vector2D> RubberConfiguration::solve()
{
	std::vector<double> sx = x.solve();
	std::vector<double> sy = y.solve();
	clear();

	std::vector<Vector2D> result(size);
	for(size_t i = 0; i < size; i++)
	{
		result[i].x = sx[i];
		result[i].y = sy[i];
	}

	return result;
}
