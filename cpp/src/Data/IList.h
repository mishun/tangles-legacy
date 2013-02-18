#pragma once

#include <cstddef>

namespace Data
{
	template<typename Element>
	class IList
	{
	protected:
		virtual ~IList() {}

	public:
		virtual size_t size() const = 0;
		virtual Element & get() = 0;
		virtual const Element & get() const = 0;

	public:
		Element operator[](size_t index)
		{
			return get(index);
		}

		const Element operator[](size_t index) const
		{
			return get(index);
		}

		template<class Function>
		void foreach(const Function & function)
		{
			const size_t s = size();
			for(size_t i = 0; i < s; i++)
				function((*this)[i]);
		}

		template<class Type, class Function>
		Type foldl(Type value, const Function & function) const
		{
			const size_t s = size();
			for(size_t i = 0; i < s; i++)
				value = function(value, get(i));
			return value;
		}

		template<class Type, class Function>
		Type foldr(const Function & function, Type value) const
		{
			const size_t s = size();
			for(size_t i = 0; i < s; i++)
				value = function(get(s - 1 - i), value);
			return value;
		}
	};
}
