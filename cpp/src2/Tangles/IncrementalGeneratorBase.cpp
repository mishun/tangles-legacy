#include <ctime>
#include "IncrementalGeneratorBase.h"

using namespace Tangles;

IncrementalGeneratorBase::IncrementalGeneratorBase(size_t mc)
	: maxCrossings(mc)
	, _total(0)
{
}

IncrementalGeneratorBase::~IncrementalGeneratorBase()
{
}

unsigned long long IncrementalGeneratorBase::generate()
{
	_total = 0;
	init();

	const double beginTime = clock();
	startGeneration();
	_totalTime = (clock() - beginTime) / CLOCKS_PER_SEC;

	done();
	return _total;
}

unsigned long long IncrementalGeneratorBase::total() const
{
	return _total;
}

double IncrementalGeneratorBase::time() const
{
	return _totalTime;
}

void IncrementalGeneratorBase::incrementalBacktrack(IncrementalTangleBase & tangle)
{
	{
		_total++;
		bool processChildren = visit(tangle);

		if(!processChildren || tangle.numberOfCrossings() >= maxCrossings)
			return;
	}

	const size_t period = tangle.getPeriod();
	if(tangle.hasMirrorSymmetry())
	{
		for(size_t gl = 3; gl >= 1; gl--)
		{
			const size_t mirrorDiff = (tangle.getMirrorDifference() + gl - 1) % period;

			const auto left = tangle.moveCW(tangle.baseLeg(), (period - mirrorDiff) / 2);
			const auto right = tangle.moveCCW(tangle.baseLeg(), 1 + mirrorDiff / 2);

			for(auto l = left; l != right; l = tangle.nextLegCCW(l))
				if(tangle.glue(gl, l))
				{
					incrementalBacktrack(tangle);
					tangle.cancellGlue();
				}
		}
	}
	else
		for(size_t gl = 3; gl >= 1; gl--)
		{
			auto l = tangle.baseLeg();
			for(size_t i = 0; i < period; i++, l = tangle.nextLegCCW(l))
				if(tangle.glue(gl, l))
				{
					incrementalBacktrack(tangle);
					tangle.cancellGlue();
				}
		}
}

void IncrementalGeneratorBase::init()
{
}

void IncrementalGeneratorBase::done()
{
}
