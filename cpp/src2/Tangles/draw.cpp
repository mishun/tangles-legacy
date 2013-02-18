#include <cassert>
#include <ctime>
#include <cmath>
#include <vector>
#include <string>
#include <iostream>
#include <stdexcept>
#include <Graph/PSPrinter.h>
#include <Geometry/Geometry.h>
#include <Topology/EmbeddedGraph.h>
#include <Draw/CircleDrawer.h>
#include "TangleGraph.h"

using namespace Graph;
using Geometry::pi;
using Geometry::Complex;
using Geometry::Vector2D;
using Topology::EmbeddedGraph;

using namespace Tangles;

static const double line_width   = 0.015;
static const double border_width = 0.01;
static const double dash_length  = 3.0;
static const double point_radius = 0.04;
static const double end_radius   = 0.03;
static const double cross_delta  = 0.06;

class TangentDrawer
{
public:
	TangentDrawer(const std::vector< std::vector<size_t> > & v, const std::vector<size_t> & e)
		: g(e.size() + 2)
		, precision(1e-6)
	{
		const size_t e_base = v.size(), f_base = v.size() + e.size() / 2 - 1;
		type.assign(g.size(), 0);

		std::vector<size_t> face(e.size(), 0);
		size_t face_id = 1;
		for(size_t i = 0; i < e.size(); i++)
		{
			if(face[i] > 0)
				continue;

			size_t cur_face = face_id++;
			for(size_t j = i; face[j] == 0; )
			{
				face[j] = cur_face;
				g[f_base + cur_face].push_back(e_base + j / 2);
				size_t vertex = e[j];
				g[f_base + cur_face].push_back(vertex);
				size_t off = 0;
				while(v[vertex] [off] != j) off++;
				off = (off + 1) % v[vertex].size();
				j = v[vertex] [off] ^ 1;
			}
			std::reverse(g[f_base + cur_face].begin(), g[f_base + cur_face].end());
			type[f_base + cur_face] = 2;
		}

		for(size_t i = 0; i < v.size(); i++)
			for(size_t j = 0; j < v[i].size(); j++)
			{
				g[i].push_back(e_base + v[i] [j] / 2);
				g[i].push_back(f_base + face[ v[i] [j] ]);
			}

		for(size_t i = 0; i < e.size(); i += 2)
		{
			g[e_base + i / 2].push_back(f_base + face[i]);
			g[e_base + i / 2].push_back(e[i]);
			g[e_base + i / 2].push_back(f_base + face[i + 1]);
			g[e_base + i / 2].push_back(e[i + 1]);
			type[e_base + i / 2] = 1;
		}

		deformation = false;
	}

	std::string render()
	{
		if(searchRad() >= precision)
			throw std::logic_error("can not find solution");

		getPositions();

		for(size_t j = 0; j < 20; j++)
		{
			Vector2D center(0, 0);
			double mass = 0;

			for(size_t i = 1; i < g.size(); i++)
			{
				if(type[i] != 0)
					continue;

				double m = 1.0 / r[i];
				center += x[i] * m;
				mass += m;
			}

			move(center / mass);
		}

		for(size_t i = 0; i < g.size(); i++)
			if(type[i] == 0 && r[i] < 0.08 && x[i].abs() < 0.4)
			{
				deformation = true;
				break;
			}

		PSPrinter::Image ps;

		ps.setLineWidth(line_width);
		for(size_t i = 1; i < g.size(); i++)
			if(type[i] == 0)
			{
				arc(ps, x[i], x[i] + (x[ g[i] [2] ] - x[i]).normalized() * r[i], x[i] + (x[ g[i] [6] ] - x[i]).normalized() * r[i]);

				Vector2D c = cross(x[i], x[i] + (x[ g[i] [2] ] - x[i]).normalized() * r[i],
					x[i] + (x[ g[i] [6] ] - x[i]).normalized() * r[i],
					x[i] + (x[ g[i] [0] ] - x[i]).normalized() * r[i],
					x[i] + (x[ g[i] [4] ] - x[i]).normalized() * r[i]);

				ps.fill(Graph::Circle(c, cross_delta), Graph::Color::White);

				arc(ps, x[i], x[i] + (x[ g[i] [0] ] - x[i]).normalized() * r[i], x[i] + (x[ g[i] [4] ] - x[i]).normalized() * r[i]);
			}
			else if(type[i] == 1)
			{
				arc(ps, x[i], (g[i] [1] == 0) ? x[i].normalized() : x[i] + (x[ g[i] [1] ] - x[i]).normalized() * r[i],
					(g[i] [3] == 0) ? x[i].normalized() : x[i] + (x[ g[i] [3] ] - x[i]).normalized() * r[i]);
			}

		ps.setLineWidth(0.0);
		for(size_t i = 1; i < g.size(); i++)
		{
			if(type[i] != 1)
				continue;

			for(size_t j = 0; j < g[i].size(); j++)
				if(g[i] [j] == 0)
				{
					Vector2D p = x[i].normalized();
					ps.fill(Graph::Circle(p, end_radius));
				}
		}

		ps.setLineWidth(border_width);
		ps.setDash(dash_length * border_width, dash_length * border_width);
		ps.stroke(Graph::Circle(Vector2D(0.0, 0.0), 1.0));

		return ps.toString();
	}

private:
	double searchRad()
	{
		r.assign(g.size(), 0.1 / g.size());
		r[0] = 1.0;

		double time = clock();
		double error;
		size_t iteration = 0;
		do
		{
			for(int i = (int)g.size() - 1; i >= 0; i--)
			{
				std::vector<double> tmp = r;
				binarySearch(i);
				double angle = getAngle(i) - 2.0 * pi;

				if(fabs(angle) > 1e-5)
					r = tmp;
			}

			double lead = r[0];
			for(size_t i = 0; i < r.size(); i++)
				r[i] /= lead;			

			error = 0.0;
			for(size_t i = 0; i < g.size(); i++)
			{
				double angle = getAngle(i) - 2.0 * pi;
				error += angle * angle;
			}
			error = sqrt(error);
			iteration++;
		} while(error > precision && iteration < 1000);

		time = (clock() - time) / CLOCKS_PER_SEC;
		std::cerr << "Found after " << iteration << " iterations; precision: " << error << " " << time << "s\n";
		return error;
	}

	void binarySearch(size_t i)
	{
		double u, d;

		if(getAngle(i) > 2.0 * pi)
			d = r[i], u = 2.0 * r[0];
		else
			d = 0.0, u = r[i];

		while(fabs(u - d) > 1e-9)
		{
			r[i] = 0.5 * (u + d);
			if(getAngle(i) > 2.0 * pi)
				d = r[i];
			else
				u = r[i];
		}
		r[i] = 0.5 * (u + d);
	}

	double getAngle(size_t u) const
	{
		double angle = 0.0;
		if(u == 0)
		{
			for(size_t i = 0; i < g[u].size(); i++)
			{
				size_t v = g[u] [i];
				size_t w = g[u] [ (i + 1) % g[u].size() ];

				double a = r[u] - r[v];
				double b = r[u] - r[w];
				double c = r[v] + r[w];

				if(a > 0.0 && b > 0.0 && c > 0.0 && c <= a + b && b <= a + c && a <= b + c)
					angle += acos((a * a + b * b - c * c) / (2 * a * b));
				else return 1e100;
			}
		}
		else
		{
			for(size_t i = 0; i < g[u].size(); i++)
			{
				size_t v = g[u] [i];
				size_t w = g[u] [ (i + 1) % g[u].size() ];
				if(w == 0)
					std::swap(v, w);

				if(v == 0)
				{
					double a = r[v] - r[u];
					double b = r[u] + r[w];
					double c = r[v] - r[w];

					if(a > 0.0 && b > 0.0 && c > 0.0 && c <= a + b && b <= a + c && a <= b + c)
						angle += acos(-(a * a + b * b - c * c) / (2 * a * b));
					else
						return -1e100;
				}
				else
				{
					double a = r[u] + r[v];
					double b = r[u] + r[w];
					double c = r[v] + r[w];
					angle += acos((a * a + b * b - c * c) / (2 * a * b));
				}
			}
		}

		return angle;
	}

	void getPositions()
	{
		x.assign(g.size(), Vector2D());
		std::vector<bool> ok(g.size(), false);
		ok[0] = true;

		for(size_t i = 1; i < g.size(); i++)
		{
			bool border = false;
			for(size_t j = 0; j < g[i].size(); j++)
				if(g[i] [j] == 0)
					border = true;

			if(border)
			{
				x[i] = Vector2D(1.0 - r[i], 0);
				ok[i] = true;
				break;
			}
		}

		while(true)
		{
			bool all = true;
			for(size_t i = 0; i < ok.size(); i++)
				if(!ok[i])
				{
					all = false;
					break;
				}

			if(all)
				break;

			for(size_t i = 0; i < g.size(); i++)
			{
				if(ok[i])
					continue;

				for(size_t j = 0; j < g[i].size(); j++)
				{
					size_t a = g[i] [j];
					size_t b = g[i] [ (j + 1) % g[i].size() ];

					if(!ok[a] || !ok[b])
						continue;

					x[i] = getCenter(i, a, b);
					ok[i] = true;
					break;
				}
			}
		}
	}

	Vector2D getCenter(size_t u, size_t v, size_t w) const
	{
		assert(u != 0 && (v != 0 || w != 0));

		if(v == 0)
			return getThirdVertex(x[w], x[v], r[w] + r[u], r[v] - r[u]);
		else if(w == 0)
			return getThirdVertex(x[w], x[v], r[w] - r[u], r[v] + r[u]);
		else
			return getThirdVertex(x[v], x[w], r[u] + r[v], r[u] + r[w]);
	}

	static Vector2D getThirdVertex(Vector2D u, Vector2D v, double a, double b)
	{
		double d = (u - v).abs();
		if(!(a + b >= d && a + d >= b && b + d >= a))
			return Vector2D(0, 0);
		double x = (d * d + a * a - b * b) / (2 * d);
		double y = sqrt(a * a - x * x);
		Vector2D xort = (v - u).normalized();
		return u + xort * x + xort.ort() * y;
	}

	static Vector2D trans(Vector2D p, Vector2D c)
	{
		Complex z(p.x, p.y), z0(c.x, c.y);
		Complex r = (z - z0) / (Complex(1.0) - z * (~z0));
		return Vector2D(r.re, r.im);
	}

	void move(Vector2D c)
	{
		for(size_t i = 1; i < g.size(); i++)
		{
			Vector2D p1 = x[i] + Vector2D(r[i], 0);
			Vector2D p2 = x[i] + Vector2D(-r[i], 0);
			Vector2D p3 = x[i] + Vector2D(0, r[i]);

			p1 = trans(p1, c);
			p2 = trans(p2, c);
			p3 = trans(p3, c);

			x[i] = Vector2D::getCircleCenter(p1, p2, p3);
			r[i] = (p1 - x[i]).abs();
		}
	}

	static std::pair<Vector2D, double> getOrtCenter(Vector2D c, Vector2D a, Vector2D b)
	{
		Vector2D center = ((a - c) + (b - c)).normalized();
		center = c + center * ((a - c).abs() / ((a - c).normalized() * center));
		return std::make_pair(center, (center - a).abs());
	}

	void arc(PSPrinter::Image & ps, Vector2D c, Vector2D a, Vector2D b) const
	{
		const size_t points = 10;

		if(fabs((a - c).normalized() * (b - c).normalized()) > 0.99)
		{
			if(deformation)
			{
				Graph::Chain chain;
				for(size_t i = 0; i <= points + 1; i++)
				{
					Vector2D p = a + (b - a) * i / (points + 1);
					p = p.normalized() * sqrt(p.abs());
					chain >> p;
				}
				ps.stroke(chain);
			}
			else
				ps.stroke(Graph::Line(a, b));
		}
		else
		{
			std::pair<Vector2D, double> tmp = getOrtCenter(c, a, b);

			Vector2D center = tmp.first;
			double r = tmp.second;

			double u = (a - center).atan2() / pi * 180.0;
			double d = (b - center).atan2() / pi * 180.0;

			if(d > u)
				std::swap(d, u);

			if(u - d >= 180.0)
			{
				d += 360.0;
				std::swap(d, u);
			}

			if(deformation)
			{
				Graph::Chain chain;
				for(size_t i = 0; i <= points + 1; i++)
				{
					Vector2D p = center + Vector2D::polar((d + (u - d) * i / (points + 1)) / 180.0 * pi) * r;
					p = p.normalized() * sqrt(p.abs());
					chain >> p;
				}
				ps.stroke(chain);
			}
			else
				ps.stroke(Graph::Arc(center, r, Geometry::Angle::fromDegrees(d), Geometry::Angle::fromDegrees(u)));
		}
	}

	Vector2D cross(Vector2D c, Vector2D a1, Vector2D b1, Vector2D a2, Vector2D b2) const
	{
		if(fabs((a1 - c).normalized() * (b1 - c).normalized()) > 0.99 && fabs((a2 - c).normalized() * (b2 - c).normalized()) > 0.99)
		{
			Vector2D p = Vector2D::intersection(a1, b1, a2, b2);
			if(deformation)
				return p.normalized() * sqrt(p.abs());
			else return p;
		}

		if(fabs((a1 - c).normalized() * (b1 - c).normalized()) <= 0.99 && fabs((a2 - c).normalized() * (b2 - c).normalized()) <= 0.99)
		{
			std::pair<Vector2D, double> tmp1 = getOrtCenter(c, a1, b1);
			std::pair<Vector2D, double> tmp2 = getOrtCenter(c, a2, b2);

			Vector2D c1 = tmp1.first, c2 = tmp2.first;
			double r1 = tmp1.second, r2 = tmp2.second, d = (c1 - c2).abs();

			double x = (d * d + r1 * r1 - r2 * r2) / (2 * d);
			double y = sqrt(r1 * r1 - x * x);

			Vector2D xort = (c2 - c1).normalized();
			Vector2D k1 = c1 + xort * x + xort.ort() * y;
			Vector2D k2 = c1 + xort * x - xort.ort() * y;

			Vector2D p;
			if((k1 - c).abs() <= (a1 - c).abs())
				p = k1;
			else
				p = k2;

			if(deformation)
				return p.normalized() * sqrt(p.abs());
			else
				return p;
		}

		if(fabs((a1 - c).normalized() * (b1 - c).normalized()) > 0.99)
		{
			std::swap(a1, a2);
			std::swap(b1, b2);
		}

		std::pair<Vector2D, double> tmp = getOrtCenter(c, a1, b1);
		Vector2D c1 = tmp.first;
		double r1 = tmp.second;

		Vector2D ort = (b2 - a2).normalized().ort();
		double x = (a2 - c1) * ort;
		double y = sqrt(r1 * r1 - x * x);

		Vector2D k1 = c1 + ort * x + ort.ort() * y;
		Vector2D k2 = c1 + ort * x - ort.ort() * y;

		Vector2D p;
		if((k1 - c).abs() <= (a1 - c).abs())
			p = k1;
		else
			p = k2;

		if(deformation)
			return p.normalized() * sqrt(p.abs());
		else
			return p;
	}

private:
	std::vector< std::vector<size_t> > g;
	std::vector<double> r;
	std::vector<Vector2D> x;
	std::vector<size_t> type;
	const double precision;
	bool deformation;
};

std::string TangleGraph::draw() const
{
	/*if(fast)
	{
		try
		{
			std::vector< std::vector<size_t> > g(v_size);
			for(size_t i = 0; i < legs_size; i++)
				g[0].push_back(legs[i]);

			for(size_t i = 1; i < v_size; i++)
				for(size_t j = 0; j < 4; j++)
					g[i].push_back(v[i].edges[j]);

			std::vector<size_t> me(e_size);
			for(size_t i = 0; i < e_size; i++)
				me[i] = e[i];

			TangentDrawer draw(g, me);
			return draw.render();
		}
		catch(std::logic_error err)
		{
			std::cerr << "Error: " << err.what() << "\n";
		}
	}*/

	EmbeddedGraph & eg = toEmbeddedGraph();
	std::string result = Draw::CircleDrawer::draw(eg, 3, true);
	eg.destroy();

	return result;
}
