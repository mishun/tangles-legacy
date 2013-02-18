#pragma once

#include "./EmbeddedGraph.h"

namespace Topology
{
	class EmbeddedTriangulation : public EmbeddedGraph
	{
	protected:
		EmbeddedTriangulation(size_t, size_t, size_t);
		virtual ~EmbeddedTriangulation();

	public:
		virtual bool isTriangulation() const;
	};
}
