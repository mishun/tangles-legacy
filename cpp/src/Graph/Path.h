#pragma once

#include <string>
#include <Geometry/Geometry.h>

namespace Graph
{
	/*class PathMark
	{
	protected:
		Geometry::Vector2D _mark;

	public:
		explicit PathMark(const Geometry::Vector2D & m)
			: _mark(m)
		{
		}

		const Geometry::Vector2D mark() const
		{
			return _mark;
		}
	};*/

	class Path
	{
	public:
		virtual ~Path() {}

		virtual bool closed() const = 0;
		virtual std::string postScriptCode() const = 0;
	};

	class Chain : public Path
	{
	protected:
		bool _closed;
		std::vector<Geometry::Vector2D> path;

	public:
		Chain();
		Chain(const Geometry::Vector2D &);

		virtual bool closed() const;
		virtual std::string postScriptCode() const;

		void append(const Geometry::Vector2D &);
		void close();
		Chain & operator>>(const Geometry::Vector2D &);
	};

	class Line : public Path
	{
	protected:
		Geometry::Vector2D begin;
		Geometry::Vector2D end;

	public:
		Line(const Geometry::Vector2D & b, const Geometry::Vector2D & e)
			: begin(b)
			, end(e)
		{
		}

		virtual bool closed() const;
		virtual std::string postScriptCode() const;
	};

	class Circle : public Path
	{
	protected:
		Geometry::Vector2D center;
		double radius;

	public:
		Circle(const Geometry::Vector2D & c, double r)
			: center(c)
			, radius(r)
		{
		}

		virtual bool closed() const;
		virtual std::string postScriptCode() const;
	};

	class Arc : public Path
	{
	protected:
		Geometry::Vector2D center;
		double radius;
		Geometry::Angle begin;
		Geometry::Angle end;

	public:
		Arc(const Geometry::Vector2D & c, double r, const Geometry::Angle & b, const Geometry::Angle & e)
			: center(c)
			, radius(r)
			, begin(b)
			, end(e)
		{
		}

		virtual bool closed() const;
		virtual std::string postScriptCode() const;
	};

	class Bezier3 : public Path
	{
	protected:
		std::vector<Geometry::Vector2D> path;

	public:
		Bezier3(const Geometry::Vector2D &);

		void add(const Geometry::Vector2D &, const Geometry::Vector2D &, const Geometry::Vector2D &);
		virtual bool closed() const;
		virtual std::string postScriptCode() const;
	};
}
