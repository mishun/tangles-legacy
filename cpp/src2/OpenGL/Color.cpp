#include "Color.h"

using namespace OpenGL;

const Color Color::Black(0.0f, 0.0f, 0.0f);
const Color Color::White(1.0f, 1.0f, 1.0f);
const Color Color::Red(1.0f, 0.0f, 0.0f);
const Color Color::Green(0.0f, 1.0f, 0.0f);
const Color Color::Blue(0.0f, 0.0f, 1.0f);

Color Color::interpolate(const Color & a, const Color & b, float alpha)
{
	if(alpha < 0.0f)
		alpha = 0.0f;

	if(alpha > 1.0f)
		alpha = 1.0f;

	float beta = 1.0f - alpha;
	return Color(
		alpha * a.r + beta * b.r,
		alpha * a.g + beta * b.g,
		alpha * a.b + beta * b.b,
		alpha * a.a + beta * b.a
		);
}
