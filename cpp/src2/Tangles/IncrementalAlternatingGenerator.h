#pragma once

#include "IncrementalAlternatingTangle.h"

namespace Tangles
{
	class IncrementalAlternatingGenerator
	{
	public:
		const size_t max_v;

	private:
		unsigned long long _total;

	public:
		IncrementalAlternatingGenerator(size_t);
		virtual ~IncrementalAlternatingGenerator();

		void generate();
		unsigned long long total() const;

	protected:
		virtual void init();
		virtual void print(const IncrementalAlternatingTangle &);
		virtual void done();

	private:
		void backtrack(IncrementalAlternatingTangle &);
	};
}
