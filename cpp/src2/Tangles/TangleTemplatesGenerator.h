#pragma once

#include "TangleTemplate.h"

namespace Tangles
{
	class TangleTemplatesGenerator
	{
	public:
		const size_t max_v;

	private:
		unsigned long long _total;
		SubTangle * loner;

	public:
		TangleTemplatesGenerator(size_t);
		virtual ~TangleTemplatesGenerator();

		void generate();
		unsigned long long total() const;
		virtual void init();
		virtual void print(const TangleTemplate &);
		virtual void done();

	private:
		void backtrack(const TangleTemplate &);
	};
}
