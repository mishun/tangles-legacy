#include <cstddef>

namespace Math { namespace KnotTh { namespace Tangles { namespace Invariants {

	void rootCode(int * rcode,
		int * queue,
		const int * connect,
		int n,
		int start,
		int dir)
	{
		int * id = new int[n + 1];
		for(int i = 0; i <= n; i++)
			id[i] = 0;
		id[start >> 2] = 1;
		int free = 2;

		queue[0] = start;
		int tail = 1;

		for(int head = 0; head < tail; head++)
		{
			int vcode = 0, cur = queue[head];
			for(int i = 0; i < 4; i++, cur = ((cur & ~3) | ((cur + dir) & 3)))
			{
				int that = connect[cur];
				vcode <<= 7;

				if(that != 0)
				{
					if(id[that >> 2] == 0)
					{
						id[that >> 2] = free++;
						queue[tail++] = that;
					}
					vcode |= id[that >> 2];
				}
			}

			rcode[head] = vcode << 3;
		}

		delete[] id;
	}

}}}}
