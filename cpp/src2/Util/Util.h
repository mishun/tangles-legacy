#pragma once

#include "IDestroyable.h"
#include "MersenneTwister.h"
#include "SharedPointer.h"
#include "AutoArray.h"
#include "IntrusivePointer.h"
#include "DisjointSets.h"
#include "ICollection.h"
#include "Numbers.h"

namespace Util
{
	class UnCopyable
	{
	public:
		UnCopyable() {}

	private:
		UnCopyable(const UnCopyable &);
		const UnCopyable & operator=(const UnCopyable &);
	};

	class UniqueObject
	{
	public:
		UniqueObject() {}

		bool operator==(const UniqueObject & that) const
		{
			return this == &that;
		}

		bool operator!=(const UniqueObject & that) const
		{
			return this != &that;
		}

	private:
		UniqueObject(const UniqueObject &);
		const UniqueObject & operator=(const UniqueObject &);
	};
}
