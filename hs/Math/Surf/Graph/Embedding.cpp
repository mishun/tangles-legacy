#include <cstddef>
#include <cmath>
#include <limits>

#include <Math/Numerical/Vector2D.h>

using Math::Numerical::Vector2D;

namespace Math { namespace Surf { namespace Graph {

	static const double kBorder = 3.0;
	static const double kElectr = 0.24;
	static const double kBend = 30.0;
	static const double kElast = 15.0;
	static const double kCross = 20.0;

	double gradient(const size_t threadsNum
		, const size_t * threadLens
		, const size_t ** threads
		, const size_t n
		, const size_t crossingsNum
		, const size_t k
		, const size_t * crossingIncidentLens
		, const size_t ** crossingIncidents
		, const Vector2D * x
		, Vector2D * g)
	{
		double * dist = new double[n];

		{
			double * q = new double[n];

			{
				for(size_t i = 0; i < n; i++)
					q[i] = 0.0;

				for(size_t ti = 0; ti < threadsNum; ti++)
				{
					const size_t len = threadLens[ti];
					const size_t * thread = threads[ti];

					for(size_t i = 0; i + 1 < len; i++)
					{
						const size_t a = thread[i];
						const size_t b = thread[i + 1];

						const double len = (x[a] - x[b]).length();
						q[a] += 0.5 * len;
						q[b] += 0.5 * len;
					}
				}
			}

			for(size_t i = 0; i < n; i++)
			{
				const double d = x[i].length();

				g[i] = x[i] * (d * log(1.0 - d) * kBorder * q[i]);
				dist[i] = d;
			}

			for(size_t ti = 0; ti < threadsNum; ti++)
			{
				const size_t len = threadLens[ti];
				const size_t * thread = threads[ti];
				const bool closed = (thread[0] == thread[len - 1]);

				for(size_t i = 0; i + 1 < len; i++)
				{
					const size_t a = thread[i];
					const size_t b = thread[i + 1];

					const auto ab = x[b] - x[a];
					const double s = ab.length();

					for(size_t j = 0; j < n; j++)
					{
						if(j == a || j == b)
							continue;

						const auto aj = x[j] - x[a];
						const auto bj = x[j] - x[b];
						const double r1 = aj.length();
						const double r2 = bj.length();

						{
							const double d = r1 + r2;
							g[j] += (aj / r1 + bj / r2) * ((s / (d * d - s * s)) * kElectr * q[j]);
						}

						{
							const double dp = ab * aj;
							const double d = (dp <= 0.0) ? r1 : ((dp >= s * s) ? r2 : fabs(ab ^ aj) / s);
							dist[j] = std::min(dist[j], d);
						}
					}

					g[a] += ab * kElast;
					g[b] -= ab * kElast;

					{
						const size_t c = (i > 0) ? thread[i - 1] : (closed ? thread[len - 2] : b);
						g[b] -= (x[b] * 3.0 - x[a] * 4.0 + x[c]) * kBend;
					}

					{
						const size_t c = (i + 2 < len) ? thread[i + 2] : (closed ? thread[1] : a);
						g[a] -= (x[a] * 3.0 - x[b] * 4.0 + x[c]) * kBend;
					}
				}
			}

			delete[] q;
		}

		for(size_t ci = 0; ci < crossingsNum; ci++)
		{
			const size_t len = crossingIncidentLens[ci];
			const size_t * inc = crossingIncidents[ci];
			const auto cr = x[ci];
			const double angle = 2.0 * acos(-1.0) / len;
			const double s = sin(angle), c = cos(angle);

			for(size_t i = 0; i < len; i++)
			{
				const auto cur = x[ inc[i] ] - cr;
				const auto prev = x[ (i == 0) ? inc[len - 1] : inc[i - 1] ] - cr;
				const auto next = x[ (i == len - 1) ? inc[0] : inc[i + 1] ] - cr;

				const Vector2D prevR(c * prev.x - s * prev.y, s * prev.x + c * prev.y);
				const Vector2D nextR(c * next.x + s * next.y, -s * next.x + c * next.y);

				const auto d = (prevR + nextR - cur * 2.0) * kCross;

				g[ inc[i] ] += d;
				g[ci] -= d;
			}
		}

		double limit = std::numeric_limits<double>::infinity();
		for(size_t i = 0; i < n; i++)
			if(i >= crossingsNum && i < crossingsNum + k)
				g[i] = 0.0;
			else
				limit = std::min(limit, fabs(dist[i] / g[i].length()));

		delete[] dist;
		return 0.3 * limit;
	}

}}}
