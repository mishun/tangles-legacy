#include <cassert>
#include <cmath>
#include "MersenneTwister.h"

using namespace Util;

MersenneTwister::MersenneTwister(unsigned long s)
	: mti(N + 1)
{
	init_genrand(s);
}

MersenneTwister::MersenneTwister(unsigned long init_key[], size_t key_length)
	: mti(N + 1)
{
	init_by_array(init_key, key_length);
}

double MersenneTwister::nextDouble()
{
	return randExc();
}

double MersenneTwister::nextNormal(double mean, double variance)
{
	double r = sqrt(-2.0 * log(1.0 - randExc())) * variance;
	double phi = 2.0 * acos(-1.0) * randExc();
	return mean + r * cos(phi);
}

size_t MersenneTwister::nextRange(size_t a, size_t b)
{
	assert(a <= b);
	return a + static_cast<size_t>((b - a + 1) * randExc());
}

void MersenneTwister::init_genrand(unsigned long s)
{
	mt[0] = s & 0xffffffffUL;
	for(mti = 1; mti < N; mti++)
	{
		mt[mti] = (1812433253UL * (mt[mti - 1] ^ (mt[mti - 1] >> 30)) + mti);
		mt[mti] &= 0xffffffffUL;
	}
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
/* slight change for C++, 2004/2/26 */
void MersenneTwister::init_by_array(unsigned long init_key[], size_t key_length)
{
	init_genrand(19650218UL);

	size_t i = 1;
	size_t j = 0;
	size_t k = ((N > key_length) ? N : key_length);

	for( ; k > 0; k--)
	{
		mt[i] = (mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 30)) * 1664525UL)) + init_key[j] + j; /* non linear */
		mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
		i++;
		j++;
		if(i >= N)
		{
			mt[0] = mt[N - 1];
			i = 1;
		}
		if(j >= key_length)
			j = 0;
	}

	for(k = N - 1; k > 0; k--)
	{
		mt[i] = (mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 30)) * 1566083941UL)) - i;
		mt[i] &= 0xffffffffUL;
		i++;
		if(i >= N)
		{
			mt[0] = mt[N-1];
			i = 1;
		}
	}

	mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
}

void MersenneTwister::update()
{
#define M		397
#define MATRIX_A	0x9908b0dfUL
#define UPPER_MASK	0x80000000UL
#define LOWER_MASK	0x7fffffffUL

	static const unsigned long mag01[2] = { 0x0UL, MATRIX_A };
	unsigned long y;
	size_t kk;

	if(mti == N + 1)
		init_genrand(5489UL);

	for(kk = 0; kk < N - M; kk++)
	{
		y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
		mt[kk] = mt[kk + M] ^ (y >> 1) ^ mag01[y & 0x1UL];
	}

	for( ; kk < N - 1; kk++)
	{
		y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
		mt[kk] = mt[kk + (M - N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
	}

	y = (mt[N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
	mt[N - 1] = mt[M - 1] ^ (y >> 1) ^ mag01[y & 0x1UL];

	mti = 0;

#undef M
#undef MATRIX_A
#undef UPPER_MASK
#undef LOWER_MASK
}

// generates a random number on [0,0xffffffff]-interval
unsigned long MersenneTwister::genrand_int32()
{
	if(mti >= N)
		update();

	unsigned long long y = mt[mti++];

	y ^= (y >> 11);
	y ^= (y << 7) & 0x9d2c5680UL;
	y ^= (y << 15) & 0xefc60000UL;
	y ^= (y >> 18);

	return y;
}

// generates a random number on [0,0x7fffffff]-interval
long MersenneTwister::genrand_int31()
{
	return (long)(genrand_int32() >> 1);
}

// generates a random number on [0,1]-real-interval
double MersenneTwister::genrand_real1()
{
	return genrand_int32() * (1.0 / 4294967295.0); 
}

// generates a random number on [0,1)-real-interval
double MersenneTwister::randExc()
{
	return genrand_int32() * (1.0 / 4294967296.0); 
}

// generates a random number on (0,1)-real-interval
double MersenneTwister::genrand_real3()
{
	return (((double)genrand_int32()) + 0.5) * (1.0 / 4294967296.0); 
}

// generates a random number on [0,1) with 53-bit resolution
double MersenneTwister::genrand_res53() 
{ 
	unsigned long a = genrand_int32() >> 5, b = genrand_int32() >> 6;
	return (a * 67108864.0 + b) * (1.0 / 9007199254740992.0);
}
