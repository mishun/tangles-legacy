#include "Image.h"
#include "psprinter.h"

using namespace Graph;
using Geometry::Vector2D;

PSPrinter::Image::Image()
{
	str.precision(4);
	str.setf(std::ios::fixed, std::ios::floatfield);
}

void PSPrinter::Image::stroke(const Path & path)
{
	str << path.postScriptCode();
	str << "stroke\n";
}

void PSPrinter::Image::fill(const Path & path)
{
	str << "gsave\n";
	str << "0 setlinewidth\n";
	str << path.postScriptCode();
	str << "fill\n";
	str << "grestore\n";
}

void PSPrinter::Image::fill(const Path & path, const Color & color)
{
	str << "gsave\n";
	str << "0 setlinewidth\n";
	str << color.red() << ' ' << color.green() << ' ' << color.blue() << " setrgbcolor\n";
	str << path.postScriptCode();
	str << "fill\n";
	str << "grestore\n";
}

void PSPrinter::Image::setScale(double factor)
{
	str << factor << ' ' << factor << " scale\n";
}

void PSPrinter::Image::setLineWidth(double w)
{
	str << w << " setlinewidth\n";
}

void PSPrinter::Image::setDash(double a, double b)
{
	str << "[" << a << " " << b << "] 0 setdash\n";
}

std::string PSPrinter::Image::toString() const
{
	return str.str();
}
