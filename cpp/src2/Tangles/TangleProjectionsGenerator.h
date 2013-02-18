#pragma once

#include "TangleProjection.h"

namespace Tangles
{
	class TangleProjectionsGenerator
	{
	public:
		const size_t max_v;

	private:
		unsigned long long _total;
		SubTangle * loner;

	public:
		TangleProjectionsGenerator(size_t);
		virtual ~TangleProjectionsGenerator();

		void generate();
		unsigned long long total() const;
		virtual void init();
		virtual void print(const TangleProjection &);
		virtual void done();

	private:
		void backtrack(const TangleProjection &);
	};
}
