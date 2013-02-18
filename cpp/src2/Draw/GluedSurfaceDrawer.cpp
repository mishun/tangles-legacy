#include <cassert>
#include <iostream>
#include "GluedSurfaceDrawer.h"

using namespace Draw;
using Topology::EmbeddedGraph;
using Geometry::Vector2D;
using Geometry::Segment2D;
using Geometry::Chain2D;

std::string GluedSurfaceDrawer::fastDraw(const Topology::EmbeddedGraph & graph)
{
	Graph::PSPrinter::Image img;
	Draw::GluedSurfaceDrawer drawer;
	drawer.draw(img, graph);
	return img.toString();
}

GluedSurfaceDrawer::GluedSurfaceDrawer()
{
}

void GluedSurfaceDrawer::draw(Graph::PSPrinter::Image & ps, const EmbeddedGraph & graph)
{
	if(graph.genus() > 0)
		drawHigherGenusGraph(ps, graph);
	else
		drawSphericGraph(ps, graph);
}


class GluedSurfaceDrawer::StarGrouper
{
protected:
	const EmbeddedGraph & star;
	size_t number_of_darts;
	size_t number_of_groups;
	std::vector< std::pair<size_t, size_t> > dart_group;
	std::vector<size_t> dart_group_size;

public:
	StarGrouper(const EmbeddedGraph &);

	size_t numberOfDarts() const;
	size_t numberOfGroups() const;
	size_t dartGroup(size_t) const;
	size_t dartIndexInGroup(size_t) const;
	size_t groupSize(size_t) const;
	size_t adjacentGroup(size_t) const;
	Segment2D groupBorderSegment(size_t) const;
	Vector2D borderPoint(double) const;
};

GluedSurfaceDrawer::StarGrouper::StarGrouper(const EmbeddedGraph & s)
	: star(s)
{
	assert(star.numberOfVertexes() == 1);
	assert(star.genus() > 0);

	const EmbeddedGraph::Vertex & star_center = star.vertex(0);
	number_of_darts = star_center.numberOfIncidentEdges();

	dart_group.assign(number_of_darts, std::pair<size_t, size_t>(0, 0));
	for(size_t cur = 0; dart_group[cur].first == 0; cur = star_center.nextIndexCCW(cur))
	{
		const size_t prev = star_center.nextIndexCW(cur);
		const size_t prev_pair = star_center.adjacentVertex(prev).place;
		const size_t cur_pair = star_center.adjacentVertex(cur).place;

		if(star_center.nextIndexCCW(cur_pair) == prev_pair)
		{
			dart_group[cur].first = dart_group[prev].first;
			dart_group[cur].second = dart_group[prev].second + 1;
		}
		else
		{
			dart_group[cur].first = dart_group[prev].first + 1;
			dart_group[cur].second = 0;
		}
	}

	number_of_groups = dart_group[number_of_darts - 1].first;

	dart_group_size.assign(number_of_groups, 0);
	for(size_t i = 0; i < number_of_darts; i++)
	{
		dart_group[i].first--;
		dart_group_size[ dart_group[i].first ]++;
	}
}

size_t GluedSurfaceDrawer::StarGrouper::numberOfDarts() const
{
	return number_of_darts;
}

size_t GluedSurfaceDrawer::StarGrouper::numberOfGroups() const
{
	return number_of_groups;
}

size_t GluedSurfaceDrawer::StarGrouper::dartGroup(size_t dart) const
{
	assert(dart < number_of_darts);
	return dart_group[dart].first;
}

size_t GluedSurfaceDrawer::StarGrouper::dartIndexInGroup(size_t dart) const
{
	assert(dart < number_of_darts);
	return dart_group[dart].second;
}

size_t GluedSurfaceDrawer::StarGrouper::groupSize(size_t group) const
{
	assert(group < number_of_groups);
	return dart_group_size[group];
}

size_t GluedSurfaceDrawer::StarGrouper::adjacentGroup(size_t group) const
{
	assert(group < number_of_groups);

	size_t result = group;
	for(size_t i = 0; i < numberOfDarts(); i++)
		if(dartGroup(i) == group)
		{
			const size_t adjacent_dart = star.vertex(0).adjacentVertex(i).place;
			result = dartGroup(adjacent_dart);
			break;
		}

	for(size_t i = 0; i < numberOfDarts(); i++)
		if(dartGroup(i) == group)
		{
			const size_t adjacent_dart = star.vertex(0).adjacentVertex(i).place;
			assert(result == dartGroup(adjacent_dart));
		}

	return result;
}

Segment2D GluedSurfaceDrawer::StarGrouper::groupBorderSegment(size_t group) const
{
	assert(group < number_of_groups);

	const double outer_radius = 1.0 / cos(Geometry::pi / numberOfGroups());
	const Vector2D a = Vector2D::polar(outer_radius, -2.0 * Geometry::pi * (group - 0.5) / numberOfGroups());
	const Vector2D b = Vector2D::polar(outer_radius, -2.0 * Geometry::pi * (group + 0.5) / numberOfGroups());

	return Segment2D(a, b);
}

Vector2D GluedSurfaceDrawer::StarGrouper::borderPoint(double index) const
{
	index = fmod(numberOfDarts() + fmod(index, numberOfDarts()), numberOfDarts());

	double iindex;
	double frac = modf(index, &iindex);

	const size_t current = (size_t)iindex;
	const size_t next = (current + 1) % numberOfDarts();

	size_t group;
	double offset;

	if(dartGroup(current) == dartGroup(next))
	{
		group = dartGroup(current);
		offset = dartIndexInGroup(current) + frac;
	}
	else
	{
		if(frac <= 0.5)
		{
			group = dartGroup(current);
			offset = dartIndexInGroup(current) + 2.0 * frac;
		}
		else
		{
			group = dartGroup(next);
			offset = 2.0 * (frac - 1.0);
		}
	}

	const size_t size = groupSize(group);

	return groupBorderSegment(group).interpolate((1.0 + offset) / (1.0 + size));
}


void GluedSurfaceDrawer::drawHigherGenusGraph(Graph::PSPrinter::Image & ps, const EmbeddedGraph & graph)
{
	std::pair< EmbeddedGraph *, EmbeddedGraph * > decomp = graph.sphereStarDecomposition();
	EmbeddedGraph & sphere = *decomp.first;
	EmbeddedGraph & star = *decomp.second;

	StarGrouper grouper(star);

	std::vector<Chain2D> embedding = derivativeEmbedding(sphere, grouper, 2, 1.0);

	ps.setScale(0.5);

	{
		ps.setLineWidth(0.01);

		{
			Graph::Chain border(grouper.groupBorderSegment(0).begin);
			for(size_t i = 0; i < grouper.numberOfGroups(); i++)
				border.append(grouper.groupBorderSegment(i).end);

			ps.stroke(border);
		}

		for(size_t i = 0; i < grouper.numberOfGroups(); i++)
		{
			Segment2D seg = grouper.groupBorderSegment(i);
			if(grouper.adjacentGroup(i) > i)
				ps.fill(Graph::Chain(seg.begin) >> seg.interpolate(0.05, 0.02) >> seg.interpolate(0.05, -0.02));
			else
				ps.fill(Graph::Chain(seg.end) >> seg.interpolate(0.95, 0.02) >> seg.interpolate(0.95, -0.02));
		}

		for(size_t i = 0; i < grouper.numberOfDarts(); i++)
			ps.fill(Graph::Circle(grouper.borderPoint(i), 0.03));
	}

	ps.setLineWidth(0.015);
	for(size_t i = 0; i < embedding.size(); i++)
	{
		const Chain2D & chain = embedding[i];

		Graph::Chain path;
		for(size_t j = 0; j < chain.size(); j++)
			path >> chain[j];
		ps.stroke(path);

		//for(size_t j = 1; j < chain.size(); j++)
		//	ps.circle(chain[j], 0.03);
	}

	ps.setDash(0.03, 0.03);
	for(size_t i = 0; i < grouper.numberOfGroups(); i++)
	{
		size_t j = grouper.adjacentGroup(i);

		if(j < i)
			continue;

		const double delta = 2.0 * Geometry::pi / grouper.numberOfGroups();

		{
			double alpha = delta * i;
			double beta = delta * j;

			if(beta - alpha > Geometry::pi + 1e-8)
			{
				beta -= 2.0 * Geometry::pi;
				std::swap(alpha, beta);
			}

			size_t cd = std::min((grouper.numberOfGroups() + j - i) % grouper.numberOfGroups(), (grouper.numberOfGroups() + i - j) % grouper.numberOfGroups());
			double dist = 1.5 + 1.5 * (double)cd / grouper.numberOfGroups();

			Vector2D a = Vector2D::polar(1.0, -alpha);
			Vector2D b = Vector2D::polar(1.0, -beta);
			Vector2D c = Vector2D::polar(dist, -0.5 * (beta + alpha));

			Vector2D d = Vector2D::polar(dist / cos(0.25 * (beta - alpha)), -(0.25 * beta + 0.75 * alpha));
			Vector2D e = Vector2D::polar(dist / cos(0.25 * (beta - alpha)), -(0.75 * beta + 0.25 * alpha));

			Graph::Bezier3 chord(a);
			chord.add(a * 2.0, d, c);
			chord.add(e, b * 2.0, b);
			ps.stroke(chord);
		}
	}

	//const Vector2D center = Vector2D::polar(1.0 / cos(0.5 * (beta - alpha)), 0.5 * (beta + alpha));
	//const double k = 180.0 / Geometry::pi;
	//ps.stroke(Graph::Arc(center, tan(0.5 * (beta - alpha)), Geometry::Angle::fromDegrees(alpha * k - 90), Geometry::Angle::fromDegrees(beta * k + 90)));

	sphere.destroy();
	star.destroy();
}

std::vector<Chain2D> GluedSurfaceDrawer::derivativeEmbedding(const EmbeddedGraph & graph, const StarGrouper & grouper, const size_t depth, const double border_step) const
{
	if(depth > 0)
	{
		EmbeddedGraph & derivative = graph.derivableGraph();
		std::vector<Chain2D> derivative_embedding = derivativeEmbedding(derivative, grouper, depth - 1, 0.5 * border_step);

		//return derivative_embedding;

		std::vector<Chain2D> embedding(graph.numberOfEdges());
		for(size_t i = 0; i < graph.numberOfEdges(); i++)
		{
			const EmbeddedGraph::Vertex & v = derivative.vertex(graph.numberOfVertexes() + graph.numberOfFaces() + i);

			Chain2D first = derivative_embedding[v.incidentEdge(0).edge.idInGraph()];
			if(v.incidentEdge(0).place == 0)
				first = first.reverse();

			Chain2D second = derivative_embedding[v.incidentEdge(2).edge.idInGraph()];
			if(v.incidentEdge(2).place == 1)
				second = second.reverse();

			embedding[i] = first + second.subChain(1, second.size() - 1);
		}

		derivative.destroy();
		return embedding;
	}

	std::vector<Vector2D> border(graph.vertex(0).numberOfIncidentEdges());
	for(size_t i = 0; i < border.size(); i++)
		border[i] = grouper.borderPoint(border_step * i);

	std::vector<Chain2D> embedding(graph.numberOfEdges());

	Geometry::RubberConfiguration rc(graph.numberOfVertexes() - 1);
	for(size_t i = 0; i < graph.numberOfEdges(); i++)
	{
		const EmbeddedGraph::Edge & e = graph.edge(i);
		size_t a = e.begin().vertex.idInGraph();
		size_t b = e.end().vertex.idInGraph();

		if(a == 0 && b == 0)
			continue;
		else if(a == 0)
			rc.connect(b - 1, border[e.begin().place], 1.0);
		else if(b == 0)
			rc.connect(a - 1, border[e.end().place], 1.0);
		else
			rc.connect(a - 1, b - 1, 0.5);
	}

	std::vector<Vector2D> coords = rc.solve();

	for(size_t i = 0; i < graph.numberOfEdges(); i++)
	{
		const EmbeddedGraph::Edge & e = graph.edge(i);
		embedding[i] += (e.begin().vertex.idInGraph() == 0) ? border[e.begin().place] : coords[e.begin().vertex.idInGraph() - 1];
		embedding[i] += (e.end().vertex.idInGraph() == 0) ? border[e.end().place] : coords[e.end().vertex.idInGraph() - 1];
	}

	return embedding;
}

void GluedSurfaceDrawer::drawSphericGraph(Graph::PSPrinter::Image & ps, const EmbeddedGraph & graph)
{
}
