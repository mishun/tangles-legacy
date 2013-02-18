#pragma once

#include <vector>
#include <map>
#include "TangleGraph.h"

namespace Tangles
{
	class GraphEquivRelation
	{
	private:
		class Element
		{
		private:
			Element * father;
			size_t rank;

		public:
			TangleGraph * graph;

		public:
			Element();
			~Element();

			bool isRepresentative() const;
			Element * findSet();
			static bool unionSet(Element *, Element *);
		};

	private:
		std::map<RootCode, Element *> elements;

	public:
		GraphEquivRelation();
		~GraphEquivRelation();

		bool declareElement(const TangleGraph &);
		bool declareEquivalence(const TangleGraph &, const TangleGraph &);
		std::vector<TangleGraph> getRepresentatives() const;

	private:
		Element * getOrCreate(const RootCode &);
	};
}
