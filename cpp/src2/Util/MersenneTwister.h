#pragma once

#include <cstddef>
#include "IRandom.h"

namespace Util
{
	class MersenneTwister : public IRandom
	{
	protected:
		static const size_t N = 624;

	protected:
		unsigned long mt[N];
		size_t mti;

	public:
		MersenneTwister(unsigned long);
		MersenneTwister(unsigned long[], size_t);

		virtual double nextDouble();
		virtual double nextNormal(double, double);
		virtual size_t nextRange(size_t, size_t);

	protected:
		void init_genrand(unsigned long);
		void init_by_array(unsigned long[], size_t);
		void update();
		unsigned long genrand_int32();
		long genrand_int31();
		double genrand_real1();
		double randExc();
		double genrand_real3();
		double genrand_res53();
	};
}
