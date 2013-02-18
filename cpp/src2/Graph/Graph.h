#pragma once

#include <Geometry/Geometry.h>
#include <Util/Util.h>

namespace Graph
{
	class Color
	{
	protected:
		double r;
		double g;
		double b;

	protected:
		Color(double _r, double _g, double _b)
			: r(_r)
			, g(_g)
			, b(_b)
		{
		}

	public:
		double red() const
		{
			return r;
		}

		double green() const
		{
			return g;
		}

		double blue() const
		{
			return b;
		}
		
		Color scale(double) const;

	public:
		static const Color Black;
		static const Color White;
		static const Color Red;
		static const Color Green;
		static const Color Blue;
	};

	class StrokeContext
	{
	protected:
		Color _color;
		double _line_width;

	public:
		void setColor(const Color & color)
		{
			_color = color;
		}
	};

	class FillContext
	{
	public:
		Color color;
	};

	/*class Image : public Util::UnCopyable
	{
	protected:
		class DrawOperation : public Util::UnCopyable
		{
		};

		class StrokeOperation : public DrawOperation
		{
		};

		class FillOperation : public DrawOperation
		{
		};

	protected:
		std::vector<DrawOperation *> operations;

	public:
		Image();
		~Image();

		void stroke(const Path &, const StrokeContext &);
		void fill(const Path &, const FillContext &);
		void fill(const Path &);
	};*/
}
