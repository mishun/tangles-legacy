#include <cstddef>
#include <cmath>
#include <algorithm>

namespace Math { namespace Numerical {

	inline double dot(const size_t n, const double * x, const double * y)
	{
		double dot = 0.0;
		for(size_t i = 0; i < n; i++)
			dot += x[i] * y[i];

		return dot;
	}

	inline void multiply(const size_t n
		, const size_t m
		, const size_t * id
		, const double * a
		, const double * x
		, double * r)
	{
		for(size_t i = 0; i < n; i++)
			r[i] = 0.0;

		for(size_t i = 0; i < m; i++)
		{
			const size_t p = id[2 * i];
			const size_t q = id[2 * i + 1];
			r[p] += a[i] * x[q];
		}
	}

	inline void diff(const size_t n
		, const size_t m
		, const size_t * id
		, const double * a
		, const double * b
		, const double * x
		, double * r)
	{
		multiply(n, m, id, a, x, r);
		for(size_t i = 0; i < n; i++)
			r[i] = b[i] - r[i];
	}

	double conjugateGradientSolve(const size_t n
		, const size_t m
		, const size_t * id
		, const double * a
		, const double * b
		, double * x)
	{
		for(size_t i = 0; i < n; i++)
			x[i] = 0.0;

		double * r = new double[n];
		diff(n, m, id, a, b, x, r);

		double * d = new double[n];
		for(size_t i = 0; i < n; i++)
			d[i] = r[i];

		double * q = new double[n];

		double delta_new = dot(n, r, r);
		const double delta0 = delta_new;
		const double eps = 1e-14;

		for(size_t k = 0; k < n && delta_new > eps * eps * delta0; k++)
		{
			multiply(n, m, id, a, d, q);
			double alpha = delta_new / dot(n, d, q);

			for(size_t i = 0; i < n; i++)
				x[i] += alpha * d[i];

			if(k % 50 == 0)
				diff(n, m, id, a, b, x, r);
			else
			{
				for(size_t i = 0; i < n; i++)
					r[i] -= alpha * q[i];
			}

			const double delta_old = delta_new;
			delta_new = dot(n, r, r);

			{
				const double beta = delta_new / delta_old;
				for(size_t i = 0; i < n; i++)
					d[i] = r[i] + beta * d[i];
			}
		}

		delete[] r;
		delete[] d;
		delete[] q; 

		return delta_new;
	}

	double gaussEliminationSolve(const size_t n
		, double ** a
		, double * b
		, double * x)
	{
		for(size_t k = 0; k < n; k++)
		{
			size_t best = k;
			for(size_t i = k + 1; i < n; i++)
				if(fabs(a[i] [k]) > fabs(a[best] [k]))
					best = i;

			if(best != k)
			{
				std::swap(a[best], a[k]);
				std::swap(b[best], b[k]);
			}

			const double * leadRow = a[k];
			const double leadElement = a[k] [k];

			for(size_t i = k + 1; i < n; i++)
			{
				const double coef = a[i] [k] / leadElement;
				for(size_t j = k; j < n; j++)
					a[i] [j] -= coef * leadRow[j];
				b[i] -= coef * b[k];
			}
		}

		for(size_t _ = 0; _ < n; _++)
		{
			const size_t i = n - 1 - _;

			double cur = b[i];
			for(size_t j = i + 1; j < n; j++)
				cur -= x[j] * a[i] [j];

			x[i] = cur / a[i] [i];
		}

		return 0.0;
	}

}}
