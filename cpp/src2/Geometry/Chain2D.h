#pragma once

#include <vector>
#include "Vector2D.h"

namespace Geometry
{
	class Chain2D
	{
	protected:
		std::vector<Vector2D> data;

	public:
		Chain2D();
		~Chain2D();

		size_t size() const;
		bool empty() const;
		const Vector2D operator[](size_t) const;
		Chain2D operator+(const Chain2D &) const;
		Chain2D operator+(const Vector2D &) const;
		Chain2D & operator+=(const Chain2D &);
		Chain2D & operator+=(const Vector2D &);
		Chain2D reverse() const;
		Chain2D subChain(size_t, size_t) const;
	};
};
