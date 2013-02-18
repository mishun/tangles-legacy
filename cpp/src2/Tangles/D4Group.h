#pragma once

#include <string>

namespace Tangles
{
	// = (E^mirror) * (C^rotation)                    
	//    |3   2|    |0   3|        |3   2|    |1   2|
	//    | \ / | C  | \ / |        | \ / | E  | \ / |
	// C: |  *  | -> |  *  |     E: |  *  | -> |  *  |
	//    | / \ |    | / \ |        | / \ |    | / \ |
	//    |0   1|    |1   2|        |0   1|    |0   3|
	class D4Group
	{
	public:
		class SubGroup
		{
		private:
			SubGroup(size_t, size_t, size_t, size_t, size_t, size_t, size_t, size_t, const char *);
			bool checkCorrectness() const;
			SubGroup(const SubGroup &);
			const SubGroup & operator=(const SubGroup &);

		public:
			size_t factorSet(D4Group g) const { return factor[g.data]; }
			size_t size()               const { return _size; }
			const SubGroup & addSymmetry(D4Group) const;
			std::string toString() const;

		private:
			size_t factor[8];
			size_t _size;
			const char * const _name;

		public:
			static const SubGroup D4;
			static const SubGroup C4;
			static const SubGroup GS;
			static const SubGroup DS;
			static const SubGroup C2;
			static const SubGroup ES;
			static const SubGroup ECS;
			static const SubGroup EC2S;
			static const SubGroup EC3S;
			static const SubGroup ID;
		};

		friend class SubGroup;

	public:
		D4Group() : data(0) {}

		D4Group(size_t rotation, bool mirror)
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

		bool operator==(D4Group b) const
		{
			return data == b.data;
		}

		std::string toString() const;

	public:
		static const D4Group identity;
		static const D4Group I;
		static const D4Group E;
		static const D4Group C;
		static const D4Group EC;
		static const D4Group CC;
		static const D4Group ECC;
		static const D4Group CCC;
		static const D4Group ECCC;

	private:
		explicit D4Group(size_t d)
			: data(d & 7)
		{
		}

	private:
		size_t data;
	};
}
