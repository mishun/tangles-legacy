#pragma once

#include <cassert>
#include "./nullptr.h"

namespace Data { namespace Native {

	template<typename Type>
	class SharedPointer
	{
	private:
		struct Data
		{
			Type * ptr;
			size_t referenceCounter;

			Data(Type * p)
				: ptr(p)
				, referenceCounter(1)
			{
			}

			~Data()
			{
				delete ptr;
				ptr = nullptr;
			}

			void addReference()
			{
				referenceCounter++;
			}

			void killReference()
			{
				referenceCounter--;
				if(referenceCounter == 0)
					delete this;
			}
		};

	private:
		Data * data;

	public:
		explicit SharedPointer()
			: data(nullptr)
		{
		}

		explicit SharedPointer(Type * ptr)
			: data((ptr == nullptr) ? nullptr : new Data(ptr))
		{
		}

		SharedPointer(const SharedPointer & that)
			: data(that.data)
		{
			if(data != nullptr)
				data->addReference();
		}

                SharedPointer(SharedPointer && that)
                	: data(that.data)
                {
                	that.data = nullptr;
                }

		~SharedPointer()
		{
			if(data != nullptr)
			{
				data->killReference();
				data = nullptr;
			}
		}

		const SharedPointer & operator=(const SharedPointer & that)
		{
			if(data != that.data)
			{
				if(data != nullptr)
					data->killReference();

				data = that.data;

				if(data != nullptr)
					data->addReference();
			}
			return *this;
		}

		const SharedPointer & operator=(SharedPointer && that)
		{
			if(data != that.data)
			{
				if(data != nullptr)
					data->killReference();

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
			return *data->ptr;
		}

		const Type & operator*() const
		{
			assert(data != nullptr);
			return *data->ptr;
		}

		Type * operator->()
		{
			assert(data != nullptr);
			return data->ptr;
		}

		const Type * operator->() const
		{
			assert(data != nullptr);
			return data->ptr;
		}
	};
} }
