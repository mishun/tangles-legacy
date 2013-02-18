#pragma once

#include <string>
#include <Topology/EmbeddedGraph.h>
#include <Geometry/Geometry.h>
#include <Graph/PSPrinter.h>

namespace Draw
{
	class CircleDrawer
	{
	public:
		static std::string draw(const Topology::EmbeddedGraph &, size_t depth, bool relax);
		static void draw(Graph::PSPrinter::Image &, const Topology::EmbeddedGraph &, size_t, bool);
		static std::vector<Geometry::Chain2D> calculateEmbedding(const Topology::EmbeddedGraph &, size_t depth = 3, bool relax = true);

	protected:
		static std::vector<Geometry::Chain2D> directCalculateEmbedding(const Topology::EmbeddedGraph &);
		static std::vector<Geometry::Chain2D> derivativeEmbedding(const Topology::EmbeddedGraph &, size_t);
		static std::vector<Geometry::Chain2D> relaxEmbedding(const Topology::EmbeddedGraph &, const std::vector<Geometry::Chain2D> &);
	};
}
