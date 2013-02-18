#pragma once

#include <cstddef>
#include <cassert>
#include <string>
#include <sstream>

namespace Algebra
{
	// = (E^mirror) * (C^rotation)                    
	//    |3   2|    |0   3|        |3   2|    |1   2|
	//    | \ / | C  | \ / |        | \ / | E  | \ / |
	// C: |  *  | -> |  *  |     E: |  *  | -> |  *  |
	//    | / \ |    | / \ |        | / \ |    | / \ |
	//    |0   1|    |1   2|        |0   1|    |0   3|
	class D4Group
	{
		friend class D4SubGroup;

	private:
		size_t data;

	private:
		explicit constexpr D4Group(size_t d)
			: data(d & 7)
		{
		}

	public:
		constexpr D4Group()
			: data(0)
		{
		}

		constexpr D4Group(size_t rotation, bool mirror)
			: data(((rotation & 3) << 1) | static_cast<size_t>(mirror))
		{
		}

		size_t rotation() const
		{
			return data >> 1;
		}

		bool mirror() const
		{
			return data & 1;
		}

		size_t map(size_t i) const
		{
			assert(i < 4);
			i = (i + rotation()) & 3;
			if(mirror() && (i & 1) != 0)
				i ^= 2;
			return i;
		}

		D4Group operator*(D4Group b) const
		{
			if(b.data & 1)
				return D4Group((b.data & 6) - (data & 6) + ((data ^ b.data) & 1));
			else
				return D4Group((b.data & 6) + (data & 6) + ((data ^ b.data) & 1));
		}

		D4Group inverse() const
		{
			if(data & 1)
				return *this;
			else
				return D4Group(-data);
		}

		bool operator==(D4Group that) const
		{
			return data == that.data;
		}

		bool operator!=(D4Group that) const
		{
			return data != that.data;
		}

		std::string toString() const
		{
			if(*this == I)
				return "I";

			std::ostringstream out;
			if(mirror())
				out << 'E';
			for(size_t i = 0; i < rotation(); i++)
				out << 'C';
			return out.str();
		}

	public:
		static const D4Group I;
		static const D4Group E;
		static const D4Group C;
		static const D4Group EC;
		static const D4Group CC;
		static const D4Group ECC;
		static const D4Group CCC;
		static const D4Group ECCC;
	};

	//template<>
	const D4Group D4Group::I   (0, false);
	const D4Group D4Group::E   (0, true );
	const D4Group D4Group::C   (1, false);
	const D4Group D4Group::EC  (1, true );
	const D4Group D4Group::CC  (2, false);
	const D4Group D4Group::ECC (2, true );
	const D4Group D4Group::CCC (3, false);
	const D4Group D4Group::ECCC(3, true );
}
