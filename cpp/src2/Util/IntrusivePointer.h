#pragma once

#include <cstddef>
#include "IDestroyable.h"

namespace Util
{
	class IntrusiveBase : public IDestroyable
	{
	private:
		size_t reference_counter;

	protected:
		IntrusiveBase()
			: reference_counter(0)
		{
		}

		virtual ~IntrusiveBase()
		{
		}
	};

	template<class Object>
	class IntrusivePointer
	{
	private:
		Object * data;

	public:
		IntrusivePointer()
		{
		}
	};
}
