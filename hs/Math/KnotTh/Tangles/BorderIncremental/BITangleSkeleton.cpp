#include <Math/KnotTh/Tangles/TangleSkeleton.h>

namespace Math { namespace KnotTh { namespace Tangles { namespace BorderIncremental {

	class BITangleSkeleton : public TangleSkeleton
	{
	protected:
		BITangleSkeleton(const size_t cn, const size_t ln)
			: TangleSkeleton(cn, ln)
		{
		}

		virtual ~BITangleSkeleton()
		{
		}

	public:
		BITangleSkeleton * glue(size_t, size_t) const;

	public:
		static BITangleSkeleton * createLoner();
	};

	BITangleSkeleton * BITangleSkeleton::glue(size_t gl, size_t leg) const
	{
		assert(gl >= 1 && gl <= 3 && gl < numberOfLegs());
		assert(leg < numberOfLegs());

		const size_t n = numberOfCrossings();
		const size_t l = numberOfLegs();
		const size_t nl = l + 4 - 2 * gl;

		BITangleSkeleton & res = *new BITangleSkeleton(n + 1, nl);

		for(size_t i = 1; i <= n; i++)
			for(size_t j = 0; j < 4; j++)
				res._c[i].neighbours[j] = _c[i].neighbours[j];

		CData & last = res._c[n + 1];

		for(size_t i = 0, j = leg; i < gl; i++, j = (j + l - 1) % l)
		{
			Dart cur = _l[j];
			last.neighbours[i] = cur;
			res._c[cur.crossing].neighbours[cur.place] = Dart(n + 1, i);
		}

		for(size_t i = 0; i < 4 - gl; i++)
		{
			size_t j = gl + i;
			last.neighbours[j] = Dart(0, i);
			res._l[i] = Dart(n + 1, j);
		}

		for(size_t i = 4 - gl, j = (leg + 1) % l; i < nl; i++, j = (j + 1) % l)
		{
			Dart cur = _l[j];
			res._l[i] = cur;
			res._c[cur.crossing].neighbours[cur.place] = Dart(0, i);
		}

		return &res;
	}

	BITangleSkeleton * BITangleSkeleton::createLoner()
	{
		BITangleSkeleton & res = *new BITangleSkeleton(1, 4);
		for(size_t i = 0; i < 4; i++)
		{
			res._l[i] = Dart(1, i);
			res._c[1].neighbours[i] = Dart(0, i);
		}

		return &res;
	}

}}}}
