#include <sstream>
#include "./Complex.h"

using namespace Geometry;

std::string Complex::toString() const
{
	std::ostringstream out;
	out << re << " + " << im << "i";
	return out.str();
}
