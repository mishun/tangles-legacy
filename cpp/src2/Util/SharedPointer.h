#pragma once

#include <cstddef>

namespace Util
{
	template<class Object>
	class SharedPointer
	{
	private:
		Object * data;
		size_t * counter;

	public:
		SharedPointer()
			: data(0)
			, counter(0)
		{
		}

		SharedPointer(Object * _data)
			: data(_data)
			, counter(new size_t)
		{
			*counter = 1;
		}

		SharedPointer(const SharedPointer & that)
			: data(that.data)
			, counter(that.counter)
		{
			if(data != 0)
				(*counter)++;
		}

		~SharedPointer()
		{
			if(data != 0)
			{
				(*counter)--;
				if(*counter == 0)
					data->destroy();
			}
		}

		const SharedPointer * operator=(const SharedPointer & that)
		{
			if(data != that.data)
			{
				if(data != 0)
				{
					(*counter)--;
					if(*counter == 0)
						data->destroy();
				}

				data = that.data;
				counter = that.counter;

				if(data != 0)
					(*counter)++;
			}
			return *this;
		}

		bool null() const
		{
			return data == 0;
		}

		Object * get()
		{
			return data;
		}

		const Object * get() const
		{
			return data;
		}

		Object & operator*()
		{
			return *data;
		}

		const Object & operator*() const
		{
			return *data;
		}

		Object * operator->()
		{
			return data;
		}

		const Object * operator->() const
		{
			return data;
		}
	};
}
