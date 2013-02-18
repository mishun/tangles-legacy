#include "./CRC.h"

namespace Data { namespace Native {

	template<typename Type, const Type poly>
	class CRCTable
	{
	public:
		Type table[256];

	public:
		explicit CRCTable()
		{
			for(size_t i = 0; i < 256; i++)
			{
				Type t = i;
				for(size_t j = 0; j < 8; j++)
					t = (t >> 1) ^ (((t & 1) == 0) ? 0 : poly);

				table[i] = t;
			}
		}

		Type next(Type crc, unsigned char byte) const
		{
			return (crc >> 8) ^ table[byte ^ static_cast<unsigned char>(crc)];
		}
	};


	template<typename Type, const Type poly, const Type init, const Type final>
	Type CRC<Type, poly, init, final>::evaluate(const size_t size, const void * _data)
	{
		static const CRCTable<Type, poly> table;

		const unsigned char * begin = static_cast<const unsigned char *>(_data);
		const unsigned char * end = begin + size;

		Type crc = init;
		for(const unsigned char * i = begin; i < end; i++)
			crc = table.next(crc, *i);

		return crc ^ final;
	}

	template<typename Type, const Type poly, const Type init, const Type final>
	Type CRC<Type, poly, init, final>::empty()
	{
		return init ^ final;
	}


	template class CRC<unsigned char, 0x8CU, 0xFFU, 0xFFU>;
	template class CRC<unsigned short, 0xA001U, 0x0000U, 0x0000U>;
	template class CRC<unsigned long, 0xEDB88320UL, 0xFFFFFFFFUL, 0xFFFFFFFFUL>;
	template class CRC<unsigned long long, 0xD800000000000000UL, 0xFFFFFFFFFFFFFFFFUL, 0xFFFFFFFFFFFFFFFFUL>;

}}
