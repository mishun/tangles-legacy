#pragma once

namespace Data
{
	template<class Function, class Type, class Iterator>
	inline Type fold(const Function & function, Type value, Iterator begin, Iterator end)
	{
		for(auto i = begin; i != end; ++i)
			value = function(value, *i);
		return value;
	}

	template<class Function, class Type, class List>
	inline Type foldl(const Function & function, Type value, const List & list)
	{
		return fold(function, value, list.cbegin(), list.cend());
	}

	template<class Function, class Type, class List>
	inline Type foldr(const Function & function, Type value, const List & list)
	{
		return fold(function, value, list.crbegin(), list.crend());
	}
}
