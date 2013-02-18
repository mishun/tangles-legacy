#pragma once

#include <cassert>
#include <cstddef>
#include "./nullptr.h"

namespace Data { namespace Native {

	class IntrusivePointerBase
	{
		template<class> friend class IntrusivePointer;

	private:
		size_t referenceCounter;

	public:
		IntrusivePointerBase()
			: referenceCounter(1)
		{
		}

	private:
		void addReference()
		{
			if(this != nullptr)
				referenceCounter++;
		}

		bool killReference()
		{
			if(this == nullptr)
				return false;
			else
			{
				referenceCounter--;
				return referenceCounter == 0;
			}
		}
	};

	template<class Type>
	class IntrusivePointer
	{
	private:
		Type * data;

	public:
		explicit IntrusivePointer()
			: data(nullptr)
		{
		}

		explicit IntrusivePointer(Type * ptr)
			: data(ptr)
		{
			data->addReference();
		}

                IntrusivePointer(const IntrusivePointer & that)
                	: data(that.data)
                {
                	data->addReference();
                }

                IntrusivePointer(IntrusivePointer && that)
                	: data(that.data)
                {
                	that.data = nullptr;
                }

                ~IntrusivePointer()
                {
                	if(data->killReference())
                		delete data;
			data = nullptr;
                }

                const IntrusivePointer & operator=(const IntrusivePointer & that)
                {
                	if(data != that.data)
                	{
                		if(data->killReference())
                			delete data;

                		data = that.data;
                		data->addReference();
                	}
                	return *this;
                }

                const IntrusivePointer & operator=(IntrusivePointer && that)
                {
                	if(data != that.data)
                	{
                		if(data->killReference())
                			delete data;

				data = that.data;
				that.data = nullptr;
                	}
                	return *this;
                }

                bool null() const
                {
                	return data == nullptr;
                }

                Type & operator*()
                {
                	assert(data != nullptr);
                	return *data;
                }

                const Type & operator*() const
                {
                	assert(data != nullptr);
                	return *data;
                }

                Type * operator->()
                {
                	assert(data != nullptr);
                	return data;
                }

                const Type * operator->() const
                {
                	assert(data != nullptr);
                	return data;
                }
	};
} }
