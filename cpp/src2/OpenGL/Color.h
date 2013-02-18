#pragma once

namespace OpenGL
{
	struct Color
	{
	public:
		float r;
		float g;
		float b;
		float a;

	public:
		Color(float _r, float _g, float _b, float _a = 1.0)
			: r(_r), g(_g), b(_b), a(_a)
		{
		}

		const float * data() const
		{
			return &r;
		}

		const Color changeAlpha(float alpha) const
		{
			return Color(r, g, b, alpha);
		}

	public:
		static Color interpolate(const Color &, const Color &, float);

	public:
		static const Color Black;
		static const Color White;
		static const Color Red;
		static const Color Green;
		static const Color Blue;
	};
}
