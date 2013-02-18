#pragma once

#include <cstddef>
#include <vector>

namespace Geometry
{
	class LinearSystem
	{
	private:
		struct AData
		{
			AData(size_t _i, size_t _j, double _value)
				: i(_i), j(_j), value(_value)
			{
			}

			size_t i;
			size_t j;
			double value;
		};

	private:
		const size_t size;
		double ** a;
		double * b;
		double * memory;
		std::vector<AData> adata;

	public:
		LinearSystem(size_t);
		~LinearSystem();

		void addA(size_t, size_t, double);
		void addB(size_t, double);
		void clear();
		std::vector<double> solve();

	protected:
		void gaussElimination();
		double conjugateGradient(double *, const double) const;
		void diff(const double *, double *) const;
		void multiply(const double *, double *) const;
		double dot(const double *, const double *) const;
	};
}
