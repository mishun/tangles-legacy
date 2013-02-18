#pragma once

#include <string>
#include <Topology/EmbeddedGraph.h>
#include <Draw/CircleDrawer.h>
#include <Tangles/TangleUtil.h>

namespace Tangles
{
	class TangleDraw
	{
	public:
		template<class Tangle>
		static std::string draw(const Tangle & tangle)
		{
			using Geometry::Vector2D;

			auto & g = toGraph(tangle);
			auto embedding = Draw::CircleDrawer::calculateEmbedding(g, 3, true);

			Graph::PSPrinter::Image ps;

			{
				ps.setLineWidth(0.015);
				for(size_t i = 0; i < g.numberOfEdges(); i++)
				{
					const auto & chain = embedding[i];
					Graph::Chain path;

					{
						const size_t s = g.edge(i).begin().vertex.idInGraph();
						const size_t p = g.edge(i).begin().place;

						if(s == 0 || ArbitraryCrossing::passOver(tangle, Dart(p, s)))
							path >> chain[0];
						else
							path >> (chain[0] + (chain[1] - chain[0]).normalized() * 0.05);
					}

					for(size_t j = 1; j + 1 < chain.size(); j++)
						path >> chain[j];

					{
						const size_t sz = chain.size() - 1;
						const size_t f = g.edge(i).end().vertex.idInGraph();
						const size_t p = g.edge(i).end().place;

						if(f == 0 || ArbitraryCrossing::passOver(tangle, Dart(p, f)))
							path >> chain[sz];
						else
							path >> (chain[sz] + (chain[sz - 1] - chain[sz]).normalized() * 0.05);
					}

					ps.stroke(path);
				}

				ps.setLineWidth(0.01);
				ps.setDash(0.03, 0.03);
				ps.stroke(Graph::Circle(Vector2D(0.0, 0.0), 1.0));
			}

			g.destroy();
			return ps.toString();
		}

		template<class Tangle>
		static std::string mpost(const Tangle & tangle)
		{
			using Geometry::Vector2D;

			auto & g = toGraph(tangle);
			auto embedding = Draw::CircleDrawer::calculateEmbedding(g, 3, true);

			std::ostringstream out;

			for(size_t i = 0; i < g.numberOfEdges(); i++)
			{
				const auto & chain = embedding[i];

				out << "e[" << i << "] := ";
				for(size_t j = 0; j < chain.size(); j++)
				{
					if(j > 0)
						out << " .. ";
					out << "(" << chain[j].x << ", " << chain[j].y << ")";
				}

				out << ";\n";
			}

			for(size_t i = 1; i < g.numberOfVertexes(); i++)
			{
				auto & v = g.vertex(i);
				auto & e = v.incidentEdge(0);

				auto & c = embedding[e.edge.idInGraph()];
				auto p = (e.place == 0) ? c[0] : c[c.size() - 1];

				out << "v[" << i << "] := (" << p.x << ", " << p.y << "); %";

				for(size_t j = 0; j < 4; j++)
					out << ((v.incidentEdge(j).place == 0) ? "+" : "-") << v.incidentEdge(j).edge.idInGraph();

				out << "\n";
			}

			g.destroy();
			return out.str();
		}
	};
}
