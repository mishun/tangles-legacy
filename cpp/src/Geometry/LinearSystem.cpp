#include <cassert>
#include <cmath>
#include <iostream>
#include "./LinearSystem.h"

using namespace Geometry;

LinearSystem::LinearSystem(size_t s)
	: size(s)
	, a(new double*[s])
	, b(new double[s])
	, memory(new double[s * s])
{
	for(size_t i = 0; i < size; i++)
		a[i] = memory + i * size;
	clear();
}

LinearSystem::~LinearSystem()
{
	delete[] a;
	delete[] b;
	delete[] memory;
}

void LinearSystem::addA(size_t i, size_t j, double value)
{
	assert(i < size);
	assert(j < size);
	a[i] [j] += value;
	adata.push_back(AData(i, j, value));
}

void LinearSystem::addB(size_t i, double value)
{
	assert(i < size);
	b[i] += value;
}

void LinearSystem::clear()
{
	for(size_t i = 0; i < size; i++)
	{
		b[i] = 0.0;
		for(size_t j = 0; j < size; j++)
			a[i] [j] = 0.0;
	}
	adata.clear();
}

std::vector<double> LinearSystem::solve()
{
	const double eps = 1e-10;

	std::vector<double> result(size);
	double error = conjugateGradient(&result[0], eps);

	if(error < eps)
		return result;
	else
	{
		gaussElimination();
		return std::vector<double>(b, b + size);
	}
}

void LinearSystem::gaussElimination()
{
	for(size_t k = 0; k < size; k++)
	{
		size_t best = k;
		for(size_t i = k + 1; i < size; i++)
			if(fabs(a[i] [k]) > fabs(a[best] [k]))
				best = i;

		if(best != k)
		{
			for(size_t i = 0; i < size; i++)
				std::swap(a[best] [i], a[k] [i]);
			std::swap(b[best], b[k]);
		}

		double lead = a[k] [k];
		for(size_t i = 0; i < size; i++)
			a[k] [i] /= lead;
		b[k] /= lead;

		for(size_t i = 0; i < size; i++)
		{
			if(i == k)
				continue;

			double lead = a[i] [k];
			for(size_t j = k; j < size; j++)
				a[i] [j] -= lead * a[k] [j];
			b[i] -= lead * b[k];
		}
	}
}

double LinearSystem::conjugateGradient(double * x, const double eps) const
{
	for(size_t i = 0; i < size; i++)
		x[i] = 0.0;

	double * r = new double[size];
	diff(x, r);

	double * d = new double[size];
	for(size_t i = 0; i < size; i++)
		d[i] = r[i];

	double * q = new double[size];		

	double delta_new = dot(r, r);
	double delta0 = delta_new;

	for(size_t k = 0; k < size && delta_new > eps * eps * delta0; k++)
	{
		multiply(d, q);
		double alpha = delta_new / dot(d, q);

		for(size_t i = 0; i < size; i++)
			x[i] += alpha * d[i];

		if(k % 50 == 0)
			diff(x, r);
		else
		{
			for(size_t i = 0; i < size; i++)
				r[i] -= alpha * q[i];
		}

		double delta_old = delta_new;
		delta_new = dot(r, r);

		double beta = delta_new / delta_old;
		for(size_t i = 0; i < size; i++)
			d[i] = r[i] + beta * d[i];
	}

	std::vector<double> result(x, x + size);

	delete[] r;
	delete[] d;
	delete[] q; 

	return delta_new;
}

void LinearSystem::diff(const double * x, double * r) const
{
	multiply(x, r);
	for(size_t i = 0; i < size; i++)
		r[i] = b[i] - r[i];
}

void LinearSystem::multiply(const double * x, double * r) const
{
	/*for(size_t i = 0; i < size; i++)
	{
		double cur = 0.0;
		for(size_t j = 0; j < size; j++)
			cur += a[i] [j] * x[j];
		r[i] = cur;
	}*/

	for(size_t i = 0; i < size; i++)
		r[i] = 0.0;

	for(size_t i = 0; i < adata.size(); i++)
		r[ adata[i].i ] += adata[i].value * x[ adata[i].j ];
}

double LinearSystem::dot(const double * x, const double * y) const
{
	double dot = 0.0;
	for(size_t i = 0; i < size; i++)
		dot += x[i] * y[i];
	return dot;
}
