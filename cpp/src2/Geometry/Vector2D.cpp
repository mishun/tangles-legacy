#include <sstream>
#include "Vector2D.h"

using namespace Geometry;

std::string Vector2D::toString() const
{
	std::ostringstream out;
	out << "(" << x << "; " << y << ")";
	return out.str();
}

double Vector2D::distToSegment(Vector2D a, Vector2D b) const
{
	b -= a;
	Vector2D c = *this - a;

	if(b * c <= 0)
		return c.abs();

	if(b * c >= b * b)
		return (c - b).abs();

	return fabs((b ^ c) / b.abs());
}

Vector2D Vector2D::getCircleCenter(Vector2D p1, Vector2D p2, Vector2D p3)
{
	double a1 = 2.0 * (p3.x - p1.x);
	double b1 = 2.0 * (p3.y - p1.y);
	double c1 = p3.abs2() - p1.abs2();

	double a2 = 2.0 * (p3.x - p2.x);
	double b2 = 2.0 * (p3.y - p2.y);
	double c2 = p3.abs2() - p2.abs2();

	double det = a1 * b2 - a2 * b1;
	return Vector2D((c1 * b2 - c2 * b1) / det, (a1 * c2 - a2 * c1) / det);
}

Vector2D Vector2D::intersection(Vector2D a1, Vector2D b1, Vector2D a2, Vector2D b2)
{
	double a11 = (b1 - a1).y, a12 = -(b1 - a1).x;
	double a21 = (b2 - a2).y, a22 = -(b2 - a2).x;
	double d1 = a1 ^ b1, d2 = a2 ^ b2;

	double det = a11 * a22 - a12 * a21;
	return Vector2D((d1 * a22 - d2 * a12) / det, (a11 * d2 - a21 * d1) / det);
}
