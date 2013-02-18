#pragma once

#include <utility>
#include <Util/Util.h>
#include "EmbeddedGraph.h"

namespace Topology
{
	class SphereStarDecompCalculator : public Util::UnCopyable
	{
	protected:
		typedef std::vector< std::vector< std::pair<size_t, size_t> > > Representation;

	protected:
		const EmbeddedGraph & graph;
		std::pair< EmbeddedGraph *, EmbeddedGraph * > result;

	public:
		SphereStarDecompCalculator(const EmbeddedGraph &);
		~SphereStarDecompCalculator();
		std::pair< EmbeddedGraph *, EmbeddedGraph * > decomposition();

	protected:
		size_t simplifyTree(Representation &, std::vector<bool> &) const;
		void edgeToSphere(Representation &, size_t) const;
		void edgeToBorder(Representation &, size_t, size_t) const;
		void generateStar(Representation &, const Representation &, const std::vector<bool> &) const;
		bool searchFacesSubset(bool[], size_t, size_t, size_t);
		bool testFaceSet(bool[], size_t);
	};
}
