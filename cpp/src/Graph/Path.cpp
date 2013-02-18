#include <sstream>
#include "./Path.h"

using namespace Graph;
using Geometry::Vector2D;

Chain::Chain()
	: _closed(false)
{
}

Chain::Chain(const Vector2D & begin)
	: _closed(false)
{
	path.push_back(begin);
}

bool Chain::closed() const
{
	return _closed;
}

std::string Chain::postScriptCode() const
{
	std::ostringstream str;

	str << "newpath\n";
	for(size_t i = 0; i < path.size(); i++)
	{
		str << path[i].x << ' ' << path[i].y << ' ';
		if(i == 0)
			str << "moveto\n";
		else
			str << "lineto\n";
	}

	if(closed())
		str << "closepath\n";

	return str.str();
}

void Chain::append(const Vector2D & mark)
{
	if(!closed())
		path.push_back(mark);
}

void Chain::close()
{
	_closed = true;
}

Chain & Chain::operator>>(const Geometry::Vector2D & mark)
{
	append(mark);
	return *this;
}


bool Line::closed() const
{
	return false;
}

std::string Line::postScriptCode() const
{
	std::ostringstream str;
	str << "newpath " << begin.x << ' ' << begin.y << " moveto " << end.x << ' ' << end.y << " lineto\n";
	return str.str();
}


bool Circle::closed() const
{
	return true;
}

std::string Circle::postScriptCode() const
{
	std::ostringstream str;
	str << "newpath " << center.x << ' ' << center.y << ' ' << radius << " 0 360 arc closepath\n";
	return str.str();
}


bool Arc::closed() const
{
	return false;
}

std::string Arc::postScriptCode() const
{
	std::ostringstream str;
	str << "newpath " << center.x << ' ' << center.y << ' ' << radius << ' ' << begin.toDegrees() << ' ' << end.toDegrees() << " arc\n";
	return str.str();
}


Bezier3::Bezier3(const Vector2D & p)
{
	path.push_back(p);
}

void Bezier3::add(const Vector2D & a, const Vector2D & b, const Vector2D & c)
{
	path.push_back(a);
	path.push_back(b);
	path.push_back(c);
}

bool Bezier3::closed() const
{
	return false;
}

std::string Bezier3::postScriptCode() const
{
	std::ostringstream str;

	str << "newpath\n";
	for(size_t i = 0; i < path.size(); i++)
	{
		str << path[i].x << ' ' << path[i].y << ' ';
		if(i == 0)
			str << "moveto\n";
		else if(i % 3 == 0)
			str << "curveto\n";
	}

	return str.str();
}
