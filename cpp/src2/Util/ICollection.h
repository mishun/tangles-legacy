#pragma once

#include <cstddef>

namespace Util
{
	template<class Type>
	class ICollection
	{
	public:
		virtual size_t size() const = 0;
		virtual size_t revision() const = 0;
	};

	template<class Type>
	class IIndexedCollection : public ICollection<Type>
	{
	public:
		virtual Type & get(size_t) = 0;
		virtual const Type & get(size_t) const = 0;

	public:
		Type & operator[](size_t index)
		{
			return get(index);
		}

		const Type & operator[](size_t index) const
		{
			return get(index);
		}
	};
}
