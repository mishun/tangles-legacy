#pragma once

#include <vector>
#include <string>
#include <algorithm>
#include <Topology/EmbeddedGraph.h>
#include "RootCode.h"

namespace Tangles
{
	class TangleGraph
	{
	public:
		struct Vertex
		{
			size_t neighbours[4];
		};

	public:
		TangleGraph();
		TangleGraph(size_t, size_t);

		size_t vertexes() const { return v.size() - 1; }
		size_t legs()     const { return l.size();     }

		TangleGraph transform(size_t, bool) const;
		TangleGraph substitute(const std::vector<TangleGraph> &) const;

		Topology::EmbeddedGraph & toEmbeddedGraph() const;

		RootCode getCode() const;
		int tryRootCode(size_t[], size_t, size_t, int) const;

		std::string draw() const;
		bool chordDiagram() const;
		size_t internalComponents() const;

		size_t border(size_t index) const { return l[index];  }

		size_t getCutpoints() const;

	private:
		size_t dfsCP(size_t, size_t &, size_t & , size_t[]) const;

	public:
		std::vector<Vertex> v;
		std::vector<size_t> l;
		size_t parts;
	/*public:
		RootCode old_code;
		size_t last_subtangle;*/
	};
}
