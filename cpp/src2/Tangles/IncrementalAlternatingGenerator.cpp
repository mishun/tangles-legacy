#include <ctime>
#include <iostream>
#include "IncrementalAlternatingGenerator.h"

using namespace Tangles;

IncrementalAlternatingGenerator::IncrementalAlternatingGenerator(size_t mv)
	: max_v(mv)
	, _total(0)
{
}

IncrementalAlternatingGenerator::~IncrementalAlternatingGenerator()
{
}

void IncrementalAlternatingGenerator::generate()
{
	_total = 0;
	init();

	const double begin_time = clock();

	IncrementalAlternatingTangle tangle;
	backtrack(tangle);

	{
		const double total_time = (clock() - begin_time) / CLOCKS_PER_SEC;
		std::cerr << "IncrementalAlternatingGenerator::generate(): " << total() << " total";
		std::cerr << " (" << total_time << "s, " << total() / total_time << "t/s)\n";
	}

	done();
}

unsigned long long IncrementalAlternatingGenerator::total() const
{
	return _total;
}

void IncrementalAlternatingGenerator::init()
{
}

void IncrementalAlternatingGenerator::print(const IncrementalAlternatingTangle &)
{
}

void IncrementalAlternatingGenerator::done()
{
}

void IncrementalAlternatingGenerator::backtrack(IncrementalAlternatingTangle & tangle)
{
	_total++;
	print(tangle);

	if(tangle.numberOfCrossings() >= max_v)
		return;

/*	const size_t period = tangle.getPeriod();
	if(tangle.hasMirrorSymmetry())
	{
		for(size_t gl = 3; gl >= 1; gl--)
		{
			const size_t mirrorDiff = period + tangle.getMirrorDifference() + gl - 1;

			for(size_t i = 0, pos = tangle.baseLeg(); i < period; i++, pos = tangle.nextLegCCW(pos))
			{
				const size_t reflection = (mirrorDiff - i) % period;
				if(i <= reflection && tangle.glue(gl, pos))
				{
					backtrack(tangle);
					tangle.cancellGlue();
				}
			}
		}
	}
	else
	{
		for(size_t gl = 3; gl >= 1; gl--)
			for(size_t i = 0, pos = tangle.baseLeg(); i < period; i++, pos = tangle.nextLegCCW(pos))
				if(tangle.glue(gl, pos))
				{
					backtrack(tangle);
					tangle.cancellGlue();
				}
	}*/
}
