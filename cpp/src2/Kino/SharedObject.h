#pragma once

#include <cstring>

namespace Kino
{
	class SharedObject
	{
		template<class> friend class SharedPointer;
	protected:
		SharedObject()
			: reference_counter(1)
		{
		}

		virtual ~SharedObject();

	private:
		SharedObject * getReference()
		{
			reference_counter++;
			return this;
		}

		void killReference()
		{
			reference_counter--;
			if(reference_counter == 0)
				delete this;
		}

	private:
		size_t reference_counter;
	};

	template<class Type>
	class SharedPointer
	{
	protected:
		SharedPointer()
			: data(0)
		{
		}

		explicit SharedPointer(Type * _data)
			: data(static_cast<SharedObject *>(_data))
		{
		}

	public:
		SharedPointer(const SharedPointer & that)
			: data(that.data->getReference())
		{
		}

		~SharedPointer()
		{
			if(data != 0)
			{
				data->killReference();
			}
		}

		const SharedPointer operator=(const SharedPointer & that)
		{
			if(data != that.data)
			{
				if(data != 0)
				{
					data->killReference();
				}
				data = that.data->getReference();
			}
			return *this;
		}

		Type & get() const
		{
			return *static_cast<Type *>(data);
		}

		Type & operator*() const
		{
			return get();
		}

		Type * operator->() const
		{
			return &get();
		}

		bool null() const
		{
			return data == 0;
		}

	private:
		SharedObject * data;
	};
}
