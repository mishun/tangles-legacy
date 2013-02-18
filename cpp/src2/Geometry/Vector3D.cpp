#include <sstream>
#include "Vector3D.h"

using namespace Geometry;

std::string Vector3D::toString() const
{
	std::ostringstream out;
	out << "(" << x << "; " << y << "; " << z << ")";
	return out.str();
}
