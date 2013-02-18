#pragma once

#include <vector>
#include "AlternatingTangle.h"

namespace Tangles
{
	class AlternatingTanglesGenerator
	{
	public:
		const size_t max_v;

	private:
		unsigned long long _total;
		unsigned long long _pregenerated;
		size_t tangle_id;
		std::vector<SubTangle *> * subtangles;
		std::vector<const AlternatingTangle *> tangles;

	public:
		AlternatingTanglesGenerator(size_t);
		virtual ~AlternatingTanglesGenerator();

		void generate();
		unsigned long long total() const;
		unsigned long long pregenerated() const;
		virtual void init();
		virtual void print(const AlternatingTangle &);
		virtual void done();

	private:
		void addSubTangle(const AlternatingTangle &);
		void apply(const AlternatingTangle &, const SubTangle &);
		void backtrack(const AlternatingTangle &);
	};
}
