#include <iostream>
#include <Tangles/AlternatingTanglesGenerator.h>
#include <Tangles/TangleProjectionsGenerator.h>
#include <Tangles/TangleTemplatesGenerator.h>
#include <Graph/psprinter.h>

Graph::PSPrinter ps("output.ps");

class AlternatingGenerator : public Tangles::AlternatingTanglesGenerator
{
private:
	size_t count[32] [32];
	size_t old[32];

public:
	AlternatingGenerator(size_t max_v)
		: AlternatingTanglesGenerator(max_v)
	{
		for(size_t i = 0; i < 32; i++)
		{
			old[i] = 0;
			for(size_t j = 0; j < 32; j++)
				count[i] [j] = 0;
		}
	}

	virtual void init()
	{
	}

	virtual void print(const Tangles::AlternatingTangle & tangle)
	{
		count[tangle.crossings()] [tangle.legs() >> 1]++;

		//if(tangle.crossings() == 4)
		//	ps.print(tangle.getGraph().draw());

		if(tangle.legs() == 4)
			old[tangle.crossings()] += 8 / tangle.getSymmetryGroup().size();
	}

	virtual void done()
	{
		std::cout << "legs\\v\t";
		for(unsigned i = 1; i <= max_v; i++)
			std::cout << i << "\t";
		std::cout << "\n";

		for(unsigned i = 4; i <= 2 * (max_v + 1); i += 2)
		{
			std::cout << i << "\t";
			for(unsigned j = 1; j <= max_v; j++)
				std::cout << count[j] [i >> 1] << "\t";
			std::cout << "\n";
		}

		std::cout << "total:\t";
		for(unsigned i = 1; i <= max_v; i++)
		{
			unsigned total = 0;
			for(unsigned j = 0; j <= max_v + 1; j++)
				total += count[i] [j];
			std::cout << total << "\t";
		}
		std::cout << "\n";

		for(size_t i = 1; i <= max_v; i++)
			std::cout << old[i] << " ";
		std::cout << "\n";
	}
};

class ProjectionsGenerator : public Tangles::TangleProjectionsGenerator
{
private:
	size_t count[32] [32];

public:
	ProjectionsGenerator(size_t max_v)
		: TangleProjectionsGenerator(max_v)
	{
		for(size_t i = 0; i < 32; i++)
			for(size_t j = 0; j < 32; j++)
				count[i] [j] = 0;
	}

	virtual void init()
	{
	}

	virtual void print(const Tangles::TangleProjection & tangle)
	{
		count[tangle.crossings()] [tangle.legs() >> 1]++;
	}

	virtual void done()
	{
		std::cout << "legs\\v\t";
		for(unsigned i = 1; i <= max_v; i++)
			std::cout << i << "\t";
		std::cout << "\n";

		for(unsigned i = 4; i <= 2 * (max_v + 1); i += 2)
		{
			std::cout << i << "\t";
			for(unsigned j = 1; j <= max_v; j++)
				std::cout << count[j] [i >> 1] << "\t";
			std::cout << "\n";
		}

		std::cout << "total:\t";
		for(unsigned i = 1; i <= max_v; i++)
		{
			unsigned total = 0;
			for(unsigned j = 0; j <= max_v + 1; j++)
				total += count[i] [j];
			std::cout << total << "\t";
		}
		std::cout << "\n";
	}
};

class TemplatesGenerator : public Tangles::TangleTemplatesGenerator
{
private:
	size_t count[32] [32];

public:
	TemplatesGenerator(size_t max_v)
		: TangleTemplatesGenerator(max_v)
	{
		for(size_t i = 0; i < 32; i++)
			for(size_t j = 0; j < 32; j++)
				count[i] [j] = 0;
	}

	virtual void init()
	{
	}

	virtual void print(const Tangles::TangleTemplate & tangle)
	{
		count[tangle.crossings()] [tangle.legs() >> 1]++;
	}

	virtual void done()
	{
		std::cout << "legs\\v\t";
		for(unsigned i = 1; i <= max_v; i++)
			std::cout << i << "\t";
		std::cout << "\n";

		for(unsigned i = 4; i <= 2 * (max_v + 1); i += 2)
		{
			std::cout << i << "\t";
			for(unsigned j = 1; j <= max_v; j++)
				std::cout << count[j] [i >> 1] << "\t";
			std::cout << "\n";
		}

		std::cout << "total:\t";
		for(unsigned i = 1; i <= max_v; i++)
		{
			unsigned total = 0;
			for(unsigned j = 0; j <= max_v + 1; j++)
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

	{
		ProjectionsGenerator generator(v);
		generator.generate();
	}

//	{
//		TemplatesGenerator generator(v);
//		generator.generate();
//	}

	return 0;
}
