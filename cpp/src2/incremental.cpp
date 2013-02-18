#include <ctime>
#include <iostream>
#include <Tangles/IncrementalTangleBase.h>
#include <Tangles/IncrementalTangleProjection.h>
#include <Tangles/IncrementalTangleTemplate.h>
#include <Tangles/IncrementalProjectionsGenerator.h>
#include <Tangles/IncrementalReducedGenerator.h>
#include <Tangles/IncrementalTemplatesGenerator.h>
#include <Topology/EmbeddedGraph.h>
#include <Graph/PSPrinter.h>
#include <Draw/CircleDrawer.h>
#include <Numerical/NaturalNumber.h>

using Tangles::IncrementalTangleBase;
using Tangles::IncrementalTangleProjection;
using Tangles::IncrementalTangleTemplate;
using Tangles::IncrementalReducedProjection;

Graph::PSPrinter ps("output.ps");

class ProjectionsGenerator : public Tangles::IncrementalProjectionsGenerator
{
private:
	size_t count[32] [32];

public:
	ProjectionsGenerator(size_t max_v)
		: IncrementalProjectionsGenerator(max_v)
	{
		for(size_t i = 0; i < 32; i++)
			for(size_t j = 0; j < 32; j++)
				count[i] [j] = 0;
	}

	virtual void init()
	{
	}

	virtual void print(const IncrementalTangleProjection & tangle)
	{
	//	size_t symmetry = tangle.getPeriod();
	//	if(!tangle.hasMirrorSymmetry())
	//		symmetry *= 2;

		count[tangle.numberOfCrossings()] [tangle.numberOfLegs() >> 1]++;// += symmetry;

	//	if(tangle.numberOfLegs() == 4 && tangle.numberOfCrossings() < 5)
	//		ps.print(Draw::CircleDrawer::draw(tangle.toGraph(), 3, true));
	}

	virtual void done()
	{
		std::cerr << "IncrementalProjectionsGenerator::generate(): " << total() << " total";
		std::cerr << " (" << time() << "s, " << total() / time() << "t/s)\n";

		std::cout << "legs\\v\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
			std::cout << i << "\t";
		std::cout << "\n";

		for(unsigned i = 4; i <= 2 * (maxCrossings + 1); i += 2)
		{
			std::cout << i << "\t";
			for(unsigned j = 1; j <= maxCrossings; j++)
				std::cout << count[j] [i >> 1] << "\t";
			std::cout << "\n";
		}

		std::cout << "total:\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
		{
			unsigned total = 0;
			for(unsigned j = 0; j <= maxCrossings + 1; j++)
				total += count[i] [j];
			std::cout << total << "\t";
		}
		std::cout << "\n";
	}
};

class TemplatesGenerator : public Tangles::IncrementalTemplatesGenerator
{
private:
	size_t count[32] [32];

public:
	TemplatesGenerator(size_t max_v)
		: IncrementalTemplatesGenerator(max_v)
	{
		for(size_t i = 0; i < 32; i++)
			for(size_t j = 0; j < 32; j++)
				count[i] [j] = 0;
	}

	virtual void init()
	{
	}

	virtual void print(const IncrementalTangleTemplate & tangle)
	{
		count[tangle.numberOfCrossings()] [tangle.numberOfLegs() >> 1]++;
	}

	virtual void done()
	{
		std::cerr << "IncrementalTemplatesGenerator::generate(): " << total() << " total";
		std::cerr << " (" << time() << "s, " << total() / time() << "t/s)\n";

		std::cout << "legs\\v\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
			std::cout << i << "\t";
		std::cout << "\n";

		for(unsigned i = 4; i <= 2 * (maxCrossings + 1); i += 2)
		{
			std::cout << i << "\t";
			for(unsigned j = 1; j <= maxCrossings; j++)
				std::cout << count[j] [i >> 1] << "\t";
			std::cout << "\n";
		}

		std::cout << "total:\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
		{
			unsigned total = 0;
			for(unsigned j = 0; j <= maxCrossings + 1; j++)
				total += count[i] [j];
			std::cout << total << "\t";
		}
		std::cout << "\n";
	}
};

class ReducedGenerator : public Tangles::IncrementalReducedGenerator
{
private:
	size_t count[32] [32];

public:
	ReducedGenerator(size_t max_v)
		: IncrementalReducedGenerator(max_v)
	{
		for(size_t i = 0; i < 32; i++)
			for(size_t j = 0; j < 32; j++)
				count[i] [j] = 0;
	}

	virtual void init()
	{
	}

	virtual void print(const IncrementalReducedProjection & tangle)
	{
		count[tangle.numberOfCrossings()] [tangle.numberOfLegs() >> 1]++;
	}

	virtual void done()
	{
		std::cerr << "IncrementalReducedGenerator::generate(): " << total() << " total";
		std::cerr << " (" << time() << "s, " << total() / time() << "t/s)\n";

		std::cout << "legs\\v\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
			std::cout << i << "\t";
		std::cout << "\n";

		for(unsigned i = 4; i <= 2 * (maxCrossings + 1); i += 2)
		{
			std::cout << i << "\t";
			for(unsigned j = 1; j <= maxCrossings; j++)
				std::cout << count[j] [i >> 1] << "\t";
			std::cout << "\n";
		}

		std::cout << "total:\t";
		for(unsigned i = 1; i <= maxCrossings; i++)
		{
			unsigned total = 0;
			for(unsigned j = 0; j <= maxCrossings + 1; j++)
				total += count[i] [j];
			std::cout << total << "\t";
		}
		std::cout << "\n";
	}
};

int main()
{
	const size_t v = 9;

//	{
//		AlternatingGenerator generator(v);
//		generator.generate();
//	}

//	{
//		ProjectionsGenerator generator(v);
//		generator.generate();
//	}

	{
		TemplatesGenerator generator(v);
		generator.generate();
	}

//	{
//		ReducedGenerator generator(v);
//		generator.generate();
//	}

	return 0;
}
