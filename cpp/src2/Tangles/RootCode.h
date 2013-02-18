#pragma once

#include <string>

namespace Tangles
{
	class RootCode
	{
	private:
		struct InternalCode
		{
		private:
			void * operator new(size_t size, size_t array)
			{
				return ::operator new(size + array * sizeof(size_t));
			}

			InternalCode(size_t sz, const size_t d[])
				: _instances(1)
				, _size(sz)
			{
				for(size_t i = 0; i < _size; i++)
					_data[i] = d[i];
			}

		public:
			InternalCode * copyCode()
			{
				_instances++;
				return this;
			}

			void deleteCode()
			{
				_instances--;
				if(_instances == 0)
					delete this;
			}

			bool operator==(const InternalCode &) const;
			bool operator<(const InternalCode &) const;

		public:
			static InternalCode * createCode(size_t, const size_t[]);

		public:
			size_t _instances;
			size_t _size;
			size_t _data[];

		private:
			static InternalCode empty_code;
		};

	public:
		RootCode()
			: data(InternalCode::createCode(0, 0))
		{
		}

		RootCode(size_t sz, const size_t d[])
			: data(InternalCode::createCode(sz, d))
		{
		}

		RootCode(const RootCode & p)
			: data(p.data->copyCode())
		{
		}

		~RootCode()
		{
			data->deleteCode();
		}

		const RootCode & operator=(const RootCode & p)
		{
			if(data != p.data)
			{
				data->deleteCode();
				data = p.data->copyCode();
			}
			return *this;
		}

		bool empty() const
		{
			return data->_size == 0;
		}

		size_t size() const
		{
			return data->_size;
		}

		const size_t * getData() const
		{
			return data->_data;
		}

		bool operator==(const RootCode & p) const
		{
			if(data == p.data)
				return true;
			return *data == *p.data;
		}

		bool operator<(const RootCode & p) const
		{
			return *data < *p.data;
		}

		std::string toString() const;

	private:
		InternalCode * data;
	};
}
