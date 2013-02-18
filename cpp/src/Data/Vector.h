#pragma once

#include <cassert>
#include <vector>
#include "./IList.h"

namespace Data
{
	template<typename Element>
	class Array : public IList<Element>
	{
	private:
		std::vector<Element> data;

	public:
		virtual size_t size() const
		{
			return data.size();
		}

		virtual Element & get(size_t index)
		{
			assert(index < data.size());
			return data[index];
		}

		virtual const Element & get(size_t index) const
		{
			assert(index < data.size());
			return data[index];
		}
	};
}
