#include "./Graph.h"

using namespace Graph;
using Geometry::Vector2D;

const Color Color::Black(0.0, 0.0, 0.0);
const Color Color::White(1.0, 1.0, 1.0);
const Color Color::Red  (1.0, 0.0, 0.0);
const Color Color::Green(0.0, 1.0, 0.0);
const Color Color::Blue (0.0, 0.0, 1.0);

Color Color::scale(double factor) const
{
	if(factor < 0.0)
		factor = 0.0;

	if(factor > 1.0)
		factor = 1.0;

	return Color(r * factor, g * factor, b * factor);
}

/*
Image::Image()
{
}

Image::~Image()
{
	for(size_t i = 0; i < operations.size(); i++)
		delete operations[i];
}

void Image::stroke(const Path & path, const StrokeContext & context)
{
//	operations.push_back();
}

void Image::fill(const Path & path, const FillContext & context)
{
//	operations.push_back();
}
*/
