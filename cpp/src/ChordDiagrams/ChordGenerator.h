#pragma once

#include "ChordDiagram.h"

namespace ChordDiagrams
{
	class ChordGenerator
	{
	public:
		const size_t chords;

	private:
		ChordDiagram cd;
		unsigned long long _total;
		const bool silent;

	public:
		ChordGenerator(size_t, bool = false);
		virtual ~ChordGenerator();

		void generate();
		unsigned long long total() const;

	protected:
		struct Symmetry
		{
			Symmetry(size_t _points, size_t _period, bool _mirror, size_t _mirror_base)
				: points(_points)
				, period(_period)
				, mirror(_mirror)
				, mirror_base(_mirror_base % _period)
			{
			}

			bool isTrivial() const
			{
				return period == points && !mirror;
			}

			size_t chunks() const
			{
				return points / period;
			}

			size_t points;
			size_t period;
			bool mirror;
			size_t mirror_base;
		};

		struct PlaceChordsContext
		{
			PlaceChordsContext(size_t _chord_len, const Symmetry & _symmetry)
				: chord_len(_chord_len)
				, symmetry(_symmetry)
			{
			}

			const size_t chord_len;
			const Symmetry & symmetry;
			size_t code[ChordDiagram::MAX];
		};

	protected:
		virtual void print(const ChordDiagram &, const Symmetry &);
		virtual bool checkChord(const ChordDiagram &, size_t, size_t, size_t) const;
		virtual void init();
		virtual void done();

		void checkSymmetry(const PlaceChordsContext &, size_t, size_t);
		void backtrack(size_t, const Symmetry &);
		void placeChords(PlaceChordsContext &, size_t, size_t, size_t, size_t);
		void matchRest(size_t, const Symmetry &);
	};
}
