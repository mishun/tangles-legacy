#pragma once

#include "TangleTree.h"
#include "RootCode.h"

namespace Tangles
{
	class TangleWithRootCode : public TangleTree
	{
	protected:
		TangleWithRootCode(const SubTangle & sub)
			: TangleTree(sub)
			, rcode()
			, sym_group(0)
			, cutpoints(0)
		{
			symmetryOrder[0] = symmetryOrder[1] = 1;
		}

		TangleWithRootCode(const TangleWithRootCode & father, const SubTangle & sub, D4Group orient, const size_t pos, const size_t gl)
			: TangleTree(father, sub, orient, pos, gl)
			, rcode()
			, sym_group(0)
			, cutpoints(1)
		{
			symmetryOrder[0] = symmetryOrder[1] = 0;
		}

		virtual ~TangleWithRootCode()
		{
		}

	public:
		const TangleWithRootCode & ancestor() const
		{
			return static_cast<const TangleWithRootCode &>(TangleTree::ancestor());
		}

		const RootCode & getCode() const
		{
			if(rcode.empty())
				rcode = calcCode();
			return rcode;
		}

		const D4Group::SubGroup & getSymmetryGroup() const
		{
			if(sym_group == 0)
				sym_group = &calcSymmetryGroup();
			return *sym_group;
		}

		size_t getCutpoints() const
		{
			if(cutpoints & 1)
				cutpoints = calcCutpoints();
			return cutpoints;
		}

		bool hasMirrorSymmetry() const
		{
			assert((symmetryOrder[0] == symmetryOrder[1]) || symmetryOrder[0] == 0 || symmetryOrder[1] == 0);
			return (symmetryOrder[0] > 0) && (symmetryOrder[1] > 0);
		}

		size_t getRotationSymmetry() const
		{
			assert((symmetryOrder[0] == symmetryOrder[1]) || symmetryOrder[0] == 0 || symmetryOrder[1] == 0);
			const size_t symmetry = std::max(symmetryOrder[0], symmetryOrder[1]);
			assert(legs() % symmetry == 0);
			return symmetry;
		}

	protected:
		virtual bool preCheck(const SubTangle &, D4Group, size_t, size_t) const;
		virtual bool postCheck() const;
		virtual const D4Group::SubGroup & calcSymmetryGroup() const;

	private:
		RootCode calcCode() const;
		size_t calcCutpoints() const;

	private:
		size_t fastCode(size_t) const;
		int tryRootCode(size_t[], size_t, size_t, int) const;
		int compareRootCode(const size_t[], size_t, size_t, int) const;
		RootCode calcSymmetryCode(D4Group) const;
		std::pair<size_t, bool> __fastcall dfsCP(size_t, size_t &, size_t &, size_t[]) const;

	private:
		mutable RootCode rcode;
		mutable const D4Group::SubGroup * sym_group;
		mutable size_t cutpoints;
		mutable size_t symmetryOrder[2];

	private:
		static const size_t vertex_table[256] [2];
	};
}
