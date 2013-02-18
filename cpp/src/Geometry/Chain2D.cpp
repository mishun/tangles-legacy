#include <cassert>
#include "./Chain2D.h"

using namespace Geometry;

Chain2D::Chain2D()
{
}

Chain2D::~Chain2D()
{
}

size_t Chain2D::size() const
{
	return data.size();
}

bool Chain2D::empty() const
{
	return data.size() == 0;
}

const Vector2D Chain2D::operator[](size_t index) const
{
	assert(index < data.size());
	return data[index];
}

Chain2D Chain2D::operator+(const Chain2D & that) const
{
	Chain2D result = *this;
	for(size_t i = 0; i < that.data.size(); i++)
		result.data.push_back(that.data[i]);
	return result;
}

Chain2D Chain2D::operator+(const Vector2D & p) const
{
	Chain2D result = *this;
	result.data.push_back(p);
	return result;
}

Chain2D & Chain2D::operator+=(const Chain2D & that)
{
	for(size_t i = 0; i < that.data.size(); i++)
		data.push_back(that.data[i]);
	return *this;
}

Chain2D & Chain2D::operator+=(const Vector2D & p)
{
	data.push_back(p);
	return *this;
}

Chain2D Chain2D::reverse() const
{
	Chain2D result;
	for(size_t i = 0; i < data.size(); i++)
		result.data.push_back(data[data.size() - 1 - i]);
	return result;
}

Chain2D Chain2D::subChain(size_t begin, size_t size) const
{
	assert(begin < data.size());
	assert(begin + size <= data.size());

	Chain2D result;
	for(size_t i = 0; i < size; i++)
		result.data.push_back(data[begin + i]);
	return result;
}
