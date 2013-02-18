#pragma once

#include <cassert>
#include <cstddef>
#include <algorithm>
#include "./nullptr.h"

namespace Data { namespace Native {

	template<typename Type>
	class SharedArray
	{
	private:
		struct Data
		{
			size_t referenceCounter;
			const size_t size;
			Type array[0];

			Data(const size_t sz)
				: referenceCounter(1)
				, size(sz)
			{
				for(size_t i = 0; i < sz; i++)
					new(array + i) Type();
			}

			Data(const size_t sz, const Type & value)
				: referenceCounter(1)
				, size(sz)
			{
				for(size_t i = 0; i < sz; i++)
					new(array + i) Type(value);
			}

			~Data()
			{
				for(size_t i = 0; i < size; i++)
					array[i].~Type();
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
		explicit SharedArray(size_t size)
			: data(new(operator new(sizeof(Data) + size * sizeof(Type))) Data(size))
		{
		}

		explicit SharedArray(size_t size, const Type & value)
			: data(new(operator new(sizeof(Data) + size * sizeof(Type))) Data(size, value))
		{
		}

		SharedArray(const SharedArray & that)
			: data(that.data)
		{
			data->addReference();
		}

		SharedArray(SharedArray && that)
			: data(that.data)
		{
			that.data = nullptr;
		}

		~SharedArray()
		{
			if(data != nullptr)
			{
				data->killReference();
				data = nullptr;
			}
		}

		const SharedArray & operator=(const SharedArray & that)
		{
			if(data != that.data)
			{
				data->killReference();
				data = that.data;
				data->addReference();
			}
			return *this;
		}

		const SharedArray & operator=(SharedArray && that)
		{
			if(data != that.data)
			{
				data->killReference();
				data = that.data;
				that.data = nullptr;
			}
			return *this;
		}

		size_t size() const
		{
			return data->size;
		}

		Type * get()
		{
			return data->array;
		}

		const Type * get() const
		{
			return data->array;
		}

		Type & operator[](size_t index)
		{
			assert(index < data->size);
			return data->array[index];
		}

		const Type & operator[](size_t index) const
		{
			assert(index < data->size);
			return data->array[index];
		}

		bool operator==(const SharedArray & that) const
		{
			if(data == that.data)
				return true;

			if(data->size != that.data->size)
				return false;

			for(size_t i = 0; i < data->size; i++)
				if(data->array[i] != that.data->array[i])
					return false;

			return true;
		}

		bool operator!=(const SharedArray & that) const
		{
			return !operator==(that);
		}

		bool operator<(const SharedArray & that) const
		{
			if(data == that.data)
				return false;

			return std::lexicographical_compare(data->array, data->array + data->size, that.data->array, that.data->array + that.data->size);
		}

		bool operator>(const SharedArray & that) const
		{
			return that.operator<(*this);
		}
	};
} }
