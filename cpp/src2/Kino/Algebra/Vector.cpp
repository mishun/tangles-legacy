#include <sstream>
#include "Vector.h"

namespace Kino
{
	template<class Element>
	const std::string Vector<2, Element>::toString() const
	{
		std::ostringstream str;
		str << '[' << x << ' ' << y << ']';
		return str.str();
	}

	template<class Element>
	const std::string Vector<3, Element>::toString() const
	{
		std::ostringstream str;
		str << '[' << x << ' ' << y << ' ' << z << ']';
		return str.str();
	}

	template<class Element>
	const std::string Vector<4, Element>::toString() const
	{
		std::ostringstream str;
		str << '[' << x << ' ' << y << ' ' << z << ' ' << w << ']';
		return str.str();
	}


	template const std::string Vector<2, float>::toString() const;
	template const std::string Vector<2, double>::toString() const;

	template const std::string Vector<3, float>::toString() const;
	template const std::string Vector<3, double>::toString() const;

	template const std::string Vector<4, float>::toString() const;
	template const std::string Vector<4, double>::toString() const;
}
