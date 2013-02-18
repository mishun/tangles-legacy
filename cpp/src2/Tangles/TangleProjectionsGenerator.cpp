#include <ctime>
#include <iostream>
#include <set>
#include "TangleProjectionsGenerator.h"

using namespace Tangles;

TangleProjectionsGenerator::TangleProjectionsGenerator(size_t mv)
	: max_v(mv)
	, _total(0)
	, loner(new SubTangle(1, D4Group::SubGroup::D4, false, false))
{
}

TangleProjectionsGenerator::~TangleProjectionsGenerator()
{
	delete loner;
}

void TangleProjectionsGenerator::generate()
{
	_total = 0;
	init();

	const double begin_time = clock();

	TangleProjection * root = TangleProjection::create(*loner);
	backtrack(*root);
	root->destroy();

	{
		const double total_time = (clock() - begin_time) / CLOCKS_PER_SEC;
		std::cerr << "TangleProjectionsGenerator::generate(): " << total() << " projections total";
		std::cerr << " (" << total_time << "s, " << total() / total_time << "t/s)\n";
	}

	done();
}

unsigned long long TangleProjectionsGenerator::total() const
{
	return _total;
}

void TangleProjectionsGenerator::init()
{
}

void TangleProjectionsGenerator::print(const TangleProjection &)
{
}

void TangleProjectionsGenerator::done()
{
}

void TangleProjectionsGenerator::backtrack(const TangleProjection & tangle)
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

		for(size_t pos = 0; pos < tangle.legs(); pos++)
		{
			TangleProjection * next = tangle.glue(*loner, D4Group::I, pos, gl);
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
