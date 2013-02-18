#include "GraphEquivRelation.h"

using namespace Tangles;

GraphEquivRelation::Element::Element()
	: father(this)
	, rank(0)
	, graph(0)
{
}

GraphEquivRelation::Element::~Element()
{
	if(graph != 0)
		delete graph;
}

bool GraphEquivRelation::Element::isRepresentative() const
{
	return father == this;
}

GraphEquivRelation::Element * GraphEquivRelation::Element::findSet()
{
	if(father != this)
		father = father->findSet();
	return father;
}

bool GraphEquivRelation::Element::unionSet(Element * a, Element * b)
{
	a = a->findSet();
	b = b->findSet();

	if(a == b)
		return false;

	if(a->rank >= b->rank)
	{
		if(a->rank == b->rank)
			a->rank++;

		delete b->graph;
		b->graph = 0;
		b->father = a;
	}
	else
	{
		delete a->graph;
		a->graph = 0;
		a->father = b;
	}

	return true;
}


GraphEquivRelation::GraphEquivRelation()
{
}

GraphEquivRelation::~GraphEquivRelation()
{
}

bool GraphEquivRelation::declareElement(const TangleGraph & g)
{
	RootCode code = g.getCode();
	Element * element = getOrCreate(code);

	if(element->graph == 0)
	{
		element->graph = new TangleGraph(g);
		return true;
	}
	else
		return false;
}

bool GraphEquivRelation::declareEquivalence(const TangleGraph & ga, const TangleGraph & gb)
{
	declareElement(ga);
	declareElement(gb);

	Element * a = getOrCreate(ga.getCode());
	Element * b = getOrCreate(gb.getCode());
	return Element::unionSet(a, b);
}

std::vector<TangleGraph> GraphEquivRelation::getRepresentatives() const
{
	std::vector<TangleGraph> representatives;
	for(std::map<RootCode, Element *>::const_iterator it = elements.begin(); it != elements.end(); ++it)
	{
		Element & element = *it->second;
		if(element.isRepresentative())
			representatives.push_back(*element.graph);
	}
	return representatives;
}

GraphEquivRelation::Element * GraphEquivRelation::getOrCreate(const RootCode & code)
{
	Element * element = elements[code];
	if(element == 0)
	{
		element = new Element();
		elements[code] = element;
	}
	return element;
}
