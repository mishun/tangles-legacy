#pragma once

#include <string>
#include <Topology/EmbeddedGraph.h>
#include <Geometry/Geometry.h>
#include <Graph/PSPrinter.h>

namespace Draw
{
	class GluedSurfaceDrawer
	{
	protected:
		class StarGrouper;

	public:
		static std::string fastDraw(const Topology::EmbeddedGraph &);

	public:
		GluedSurfaceDrawer();

		void draw(Graph::PSPrinter::Image &, const Topology::EmbeddedGraph &);

	protected:
		void drawHigherGenusGraph(Graph::PSPrinter::Image &, const Topology::EmbeddedGraph &);
		void drawSphericGraph(Graph::PSPrinter::Image &, const Topology::EmbeddedGraph &);

		std::vector<Geometry::Chain2D> derivativeEmbedding(const Topology::EmbeddedGraph &, const StarGrouper &, size_t, double) const;
	};
}
