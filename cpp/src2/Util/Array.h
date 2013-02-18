#pragma once

#include "ICollection.h"

namespace Util
{
	template<class Type>
	class Array : public IIndexedCollection<Type>
	{
	};
}
