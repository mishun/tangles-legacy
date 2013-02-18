#include <sstream>
#include "./Segment2D.h"

using namespace Geometry;

std::string Segment2D::toString() const
{
	std::ostringstream out;
	out << begin.toString() << " -- " << end.toString();
	return out.str();
}
