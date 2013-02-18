#include <sstream>
#include "./Angle.h"

using namespace Geometry;

const double Angle::pi = acos(-1.0);

std::string Angle::toString() const
{
	std::ostringstream out;
	out << toDegrees() << " deg";
	return out.str();
}
