#pragma once

#include "IncrementalTangleBase.h"

namespace Tangles
{
	class IncrementalGeneratorBase
	{
	public:
		const size_t maxCrossings;

	private:
		unsigned long long _total;
		double _totalTime;

	public:
		IncrementalGeneratorBase(size_t maxCrossings);
		virtual ~IncrementalGeneratorBase();

		unsigned long long generate();
		unsigned long long total() const;
		double time() const;

	protected:
		void incrementalBacktrack(IncrementalTangleBase &);

	protected:
		virtual void startGeneration() = 0;
		virtual bool visit(const IncrementalTangleBase &) = 0;
		virtual void init();
		virtual void done();
	};
}
