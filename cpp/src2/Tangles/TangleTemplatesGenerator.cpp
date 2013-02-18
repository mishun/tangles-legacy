#include <ctime>
#include <iostream>
#include <set>
#include "TangleTemplatesGenerator.h"

using namespace Tangles;

TangleTemplatesGenerator::TangleTemplatesGenerator(size_t mv)
	: max_v(mv)
	, loner(new SubTangle(1, D4Group::SubGroup::D4, false, false))
{
}

TangleTemplatesGenerator::~TangleTemplatesGenerator()
{
	delete loner;
}

void TangleTemplatesGenerator::generate()
{
	_total = 0;
	init();

	const double begin_time = clock();

	TangleTemplate * root = TangleTemplate::create(*loner);
	backtrack(*root);
	root->destroy();

	{
		const double total_time = (clock() - begin_time) / CLOCKS_PER_SEC;
		std::cerr << "TangleTemplatesGenerator::generate(): " << total() << " templates total";
		std::cerr << " (" << total_time << "s, " << total() / total_time << "t/s)\n";
	}

	done();
}

unsigned long long TangleTemplatesGenerator::total() const
{
	return _total;
}

void TangleTemplatesGenerator::init()
{
}

void TangleTemplatesGenerator::print(const TangleTemplate &)
{
}

void TangleTemplatesGenerator::done()
{
}

void TangleTemplatesGenerator::backtrack(const TangleTemplate & tangle)
{
	_total++;
	print(tangle);

	if(tangle.crossings() >= max_v)
		return;

	std::set<RootCode> code_list;
	for(size_t gl = 3; gl >= 1; gl--)
	{
		if(tangle.legs() + 4 - 2 * gl < 4)
			continue;

		if(tangle.legs() == 4 && gl == 2)
			continue;

		for(size_t pos = 0; pos < tangle.legs(); pos++)
		{
			TangleTemplate * next = tangle.glue(*loner, D4Group::I, pos, gl);
			if(next == 0)
				continue;

			const RootCode & code = next->getCode();
			if(code_list.count(code) == 0)
			{
				code_list.insert(code);
				backtrack(*next);
			}

			next->destroy();
		}
	}
}
