#include <sstream>
#include "RootCode.h"

using namespace Tangles;

RootCode::InternalCode RootCode::InternalCode::empty_code(0, 0);

bool RootCode::InternalCode::operator==(const RootCode::InternalCode & p) const
{
	if(_size != p._size)
		return false;

	for(size_t i = 0; i < _size; i++)
		if(_data[i] != p._data[i])
			return false;

	return true;
}

bool RootCode::InternalCode::operator<(const RootCode::InternalCode & p) const
{
	size_t i = 0;
	for( ; i < _size && i < p._size; i++)
	{
		if(_data[i] < p._data[i])
			return true;
		if(_data[i] > p._data[i])
			return false;
	}
	return _size == i && i < p._size;
}

RootCode::InternalCode * RootCode::InternalCode::createCode(size_t size, const size_t data[])
{
	if(size == 0)
		return empty_code.copyCode();

	return new(size) InternalCode(size, data);
}

std::string RootCode::toString() const
{
	std::ostringstream out;
	out << '{';
	for(size_t i = 0; i < size(); i++)
		out << getData()[i] << ' ';
	out << '}';
	return out.str();
}
