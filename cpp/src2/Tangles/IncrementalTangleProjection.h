#pragma once

#include "IncrementalTangleBase.h"

namespace Tangles
{
	class IncrementalTangleProjection : public IncrementalTangleBase
	{
	private:
		struct StateDump
		{
			size_t cutpointsMask;
			size_t period;
			bool hasMirrorSymmetry;
			size_t mirrorDiff;
		};

		struct DfsResult
		{
			size_t fup;
			size_t border;
		};

	private:
		StateDump dumpStack[maxSize];

	public:
		IncrementalTangleProjection();
		virtual ~IncrementalTangleProjection();

		virtual size_t getPeriod() const;
		virtual bool hasMirrorSymmetry() const;
		virtual size_t getMirrorDifference() const;

	public:
		size_t getCutpoints() const
		{
			return dumpStack[stackDepth].cutpointsMask;
		}

	protected:
		virtual bool preCheck(size_t legsToGlue, LegIndex posToGlue);
		virtual bool postCheck(size_t legsToGlue);

	private:
		DfsResult dfs(size_t, size_t, size_t &, size_t &, size_t[]) const;
		size_t fastCode(size_t) const;
		size_t fastDirectionMask(size_t) const;
		int tryRootCode(size_t[], Dart, int) const;
		int compareRootCode(const size_t[], Dart, int) const;

	private:
		static const size_t vertex_table[256] [2];
	};
}
