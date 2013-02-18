#include <iostream>
#include <vector>
#include <set>
#include <map>
#include <Tangles/IncrementalGenerator.h>
#include <Tangles/IncrementalTangle.h>
#include <Tangles/IncrementalCanonicalTangle.h>
#include <Tangles/IncrementalFastTangle.h>
#include <Tangles/IncrementalTemplateTangle.h>
#include <Tangles/Crossings/ProjectionCrossing.h>
#include <Tangles/Crossings/ArbitraryCrossing.h>
#include <Tangles/TangleUtil.h>
#include <Tangles/TangleStorage.h>
#include <Tangles/Reidemeister.h>
#include <Data/EquivalenceRelationMap.h>
#include <Data/IntrusivePointer.h>
#include <Graph/PSPrinter.h>
#include <TangleDraw/TangleDraw.h>
#include <Algebra/Polynomial.h>
#include <Tangles/JonesPolynomial.h>

Graph::PSPrinter ps("output.ps");

template<template<typename> class T, class C, const bool triangle = false>
class Generator : public Tangles::IncrementalGenerator<T, C, triangle>
{
public:
	using Tangles::IncrementalGenerator<T, C, triangle>::maxCrossings;
	using Tangles::IncrementalGenerator<T, C, triangle>::total;
	using Tangles::IncrementalGenerator<T, C, triangle>::time;

private:
	std::map< std::pair<size_t, size_t>, unsigned long long > count;
	//std::set< Data::SharedArray<size_t> > count[32] [32];

public:
	Generator(size_t maxV)
		: Tangles::IncrementalGenerator<T, C, triangle>(maxV)
	{
	}

	virtual bool visit(const Tangles::IncrementalTangle<C> & tangle) //override
	{
		count[ std::make_pair(tangle.numberOfCrossings(), tangle.numberOfLegs()) ]++;

		//{
		//	auto reduction = Tangles::Reidemeister::greedyReidemeisterReduction(tangle);
		//	if(reduction.null() || reduction->numberOfCrossings() < tangle.numberOfCrossings())
		//		return false;
		//}

		/*auto inv = Tangles::TangleUtil::homeomorphismInvariant(tangle);
		auto & s = count[tangle.numberOfCrossings()] [tangle.numberOfLegs() >> 1];

		assert(s.count(inv) == 0);
		s.insert(inv);

		{
			auto tst = Tangles::toStorage(tangle);
			assert(Tangles::TangleUtil::homeomorphismInvariant(*tst) == Tangles::TangleUtil::homeomorphismInvariant(tangle));
		}*/

		return true;
	}

	virtual void init() //override
	{
	}

	virtual void done() //override
	{
		std::cerr << "Generator::generate(): " << total() << " total";
		std::cerr << " (" << time() << "s, " << total() / time() << "t/s)\n";

		std::cout << "legs\\v\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
			std::cout << i << "\t";
		std::cout << "\n";

		for(unsigned i = 4; i <= 2 * (maxCrossings + 1); i += 2)
		{
			std::cout << i << "\t";
			for(unsigned j = 1; j <= maxCrossings; j++)
				std::cout << count[ std::make_pair(j, i) ] << "\t";
			std::cout << "\n";
		}

		std::cout << "total:\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
		{
			unsigned long long total = 0;
			for(unsigned j = 4; j <= 2 * (maxCrossings + 1); j += 2)
				total += count[ std::make_pair(i, j) ];
			std::cout << total << "\t";
		}
		std::cout << "\n";
	}
};

class ArbitraryGenerator : public Tangles::IncrementalGenerator<Tangles::IncrementalCanonicalTangle, Tangles::ArbitraryCrossing, true>
{
private:
	struct DiagramInfo
	{
	public:
		typedef Data::SharedPointer< Tangles::TangleStorage<Tangles::ArbitraryCrossing> > DiagramPtr;

	private:
		size_t crossings;
		bool alternating;
		DiagramPtr diagram;

	public:
		DiagramInfo(const DiagramPtr & dg, const bool interesting)
			: crossings(dg.null() ? 0 : dg->numberOfCrossings())
			, alternating(dg.null() ? false : Tangles::alternating(*dg))
			, diagram(interesting ? dg : DiagramPtr())
		{
		}

		bool good() const
		{
			return !diagram.null();
		}

		const DiagramPtr & getDiagramPtr() const
		{
			assert(!diagram.null());
			return diagram;
		}

		bool operator<(const DiagramInfo & that) const
		{
			if(crossings < that.crossings)
				return true;
			else if(crossings > that.crossings)
				return false;

			if(that.diagram.null())
				return true;

			if(diagram.null())
				return false;

			if(alternating)
				return true;

			if(that.alternating)
				return false;

			return true;
		}
	};

	struct Merger
	{
		DiagramInfo operator()(const DiagramInfo & a, const DiagramInfo & b) const
		{
			if(b < a)
				return b;
			else
				return a;
		}
	};

private:
	Data::EquivalenceRelationMap< Data::SharedArray<size_t>, DiagramInfo, Merger > classes;

public:
	ArbitraryGenerator(size_t maxV)
		: IncrementalGenerator(maxV)
	{
	}

	virtual bool visit(const Tangles::IncrementalTangle<Tangles::ArbitraryCrossing> & tangle) //override
	{
		if(tangle.numberOfLegs() != 4)
			return true;

		auto inv = Tangles::diskHomeomorphismInvariant(tangle);
		auto diagram = Tangles::toStorage(tangle);

		auto reduction = Tangles::Reidemeister::greedyReidemeisterReduction(tangle);
		//auto reduction = Tangles::Reidemeister::greedyWeakReduction(tangle);

		classes.merge(inv, DiagramInfo(diagram, !reduction.null()));

		auto inv0 = Data::SharedArray<size_t>(0);

		{
			if(!reduction.null())
			{
				auto ri = Tangles::diskHomeomorphismInvariant(*reduction);

				if(!classes.contains(ri))
					classes.insert(ri, DiagramInfo(reduction, false));

				classes.join(inv, ri);
			}
			else
				classes.join(inv, inv0);
		}

		{
			auto n = Tangles::Reidemeister::neighbours3rdReidemeister(tangle);
			for(auto j = n.begin(); j != n.end(); ++j)
			{
				if(j->null())
					classes.join(inv, inv0);
				else
				{
					auto ni = Tangles::diskHomeomorphismInvariant(**j);

					if(!classes.contains(ni))
						classes.insert(ni, DiagramInfo(*j, false));

					classes.join(inv, ni);
				}
			}
		}

		return true;
	}

	virtual void init() //override
	{
		classes.insert(Data::SharedArray<size_t>(0), DiagramInfo(DiagramInfo::DiagramPtr(), false));
	}

	virtual void done() //override
	{
		std::cerr << "generation done\n";
		print();
	}

	void print()
	{
		std::map< std::pair<size_t, size_t>, size_t > table;

		for(auto it = classes.begin(); it != classes.end(); ++it)
		{
			if(!it->good())
				continue;

			auto & tangle = *(it->getDiagramPtr());
			table[ std::make_pair(tangle.numberOfCrossings(), tangle.numberOfLegs()) ]++;

			//if(tangle.numberOfCrossings() == 6 && tangle.numberOfLegs() == 4)
			//	ps.print(Tangles::TangleDraw::draw(tangle));

			/*if(tangle.numberOfCrossings() == 4 && tangle.numberOfLegs() == 4)
			{
				auto jones = Tangles::JonesPolynomial::jonesPolynomial(tangle);
				std::cout << "(" << jones.first.toString() << "; " << jones.second.toString() << ")\n";

				ps.print(Tangles::TangleDraw::draw(tangle));
			}*/
		}

		std::cerr << "ArbitraryGenerator: " << total() << " total";
		std::cerr << " (" << time() << "s, " << total() / time() << "t/s)\n";

		std::cout << "legs\\v\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
			std::cout << i << "\t";
		std::cout << "\n";

		for(unsigned i = 4; i <= 2 * (maxCrossings + 1); i += 2)
		{
			std::cout << i << "\t";
			for(unsigned j = 1; j <= maxCrossings; j++)
				std::cout << table[std::make_pair(j, i)] << "\t";
			std::cout << "\n";
		}

		std::cout << "total:\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
		{
			unsigned total = 0;
			for(unsigned j = 4; j <= 2 * (maxCrossings + 1); j += 2)
				total += table[std::make_pair(i, j)];
			std::cout << total << "\t";
		}
		std::cout << "\n";
	}
};

int main()
{
	using Tangles::ArbitraryCrossing;
	using Tangles::ProjectionCrossing;
	using Tangles::IncrementalCanonicalTangle;
	using Tangles::IncrementalFastTangle;
	using Tangles::IncrementalTemplateTangle;

//	Generator<FastTangle, ProjectionCrossing, true>(11).generate();
//	Generator<CanonicalTangle, ProjectionCrossing, true>(11).generate();
//	Generator<TemplateTangle, ProjectionCrossing>(10).generate();
//	Generator<IncrementalCanonicalTangle, ArbitraryCrossing, false>(7).generate();

	ArbitraryGenerator(9).generate();

	/*{
		using Algebra::Polynomial;

		Polynomial<int> p = Polynomial<int>::constant(5) * Polynomial<int>::invx(6);
		std::cout << p.toString() << "\n";
	}*/

	return 0;
}
