#pragma once

namespace Algebra
{
	class Numbers
	{
	public:
		static size_t greatestCommonDivisor(size_t a, size_t b)
		{
			while(a != 0 && b != 0)
				if(a >= b)
					a %= b;
				else
					b %= a;

			return a + b;
		}
	};
}
