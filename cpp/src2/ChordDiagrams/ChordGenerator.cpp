#include <cassert>
#include <cstring>
#include <ctime>
#include <iostream>
#include "ChordGenerator.h"

using namespace ChordDiagrams;

ChordGenerator::ChordGenerator(size_t _n, bool _silent)
	: chords(_n)
	, cd(_n)
	, _total(0)
	, silent(_silent)
{
}

ChordGenerator::~ChordGenerator()
{
}

void ChordGenerator::generate()
{
	_total = 0;
	init();

	double time = 0;
	if(!silent)
		time = clock();

	backtrack(1, Symmetry(cd.points, 1, true, 0));

	if(!silent)
	{
		time = (clock() - time) / CLOCKS_PER_SEC;
		std::cerr << "ChordGenerator::generate(): " << total() << " diagrams with " << chords << " chords total";
		std::cerr << " (" << time << "s, " << total() / time << "d/s)\n";
	}

	done();
}

unsigned long long ChordGenerator::total() const
{
	return _total;
}

void ChordGenerator::print(const ChordDiagram & cd, const Symmetry &)
{
	assert(cd.isFull());
}

bool ChordGenerator::checkChord(const ChordDiagram & cd, const size_t len, const size_t u, const size_t v) const
{
	return true;
}

void ChordGenerator::init()
{
}

void ChordGenerator::done()
{
}

void ChordGenerator::checkSymmetry(const PlaceChordsContext & context, const size_t code_len, const size_t new_period)
{
	if(context.symmetry.mirror)
	{
		size_t base = (context.symmetry.mirror_base + cd.points - context.chord_len) % context.symmetry.period;

		size_t begin = code_len - 1;
		while(context.code[(begin + 1) % code_len] <= base)
			begin = (begin + 1) % code_len;

		size_t tmp[ChordDiagram::MAX];

		size_t index = 0;
		bool flip = false;
		for(size_t i = 0; i < code_len; i++)
		{
			const size_t value = context.code[ (begin + code_len - i) % code_len ];

			if(flip && (value <= base || value == context.symmetry.period))
			{
				tmp[index] = context.symmetry.period;
				index++;
				flip = false;
			}

			if(value < context.symmetry.period)
			{
				const size_t rev = (base + context.symmetry.period - value) % context.symmetry.period;
				tmp[index] = rev;
				index++;
			}
			else
				flip = true;
		}

		if(flip)
			tmp[index] = context.symmetry.period;

		for(size_t i = 0; i < code_len; )
		{
			int compare = 0;

			size_t j = 0, lyn = 0;
			for( ; j < code_len; j++)
			{
				const size_t cur = tmp[(i + j) % code_len];
				const size_t prev = tmp[(i + j - lyn) % code_len];

				if(compare == 0)
				{
					if(cur < context.code[j])
						return;
					if(cur > context.code[j])
						compare = 1;
				}

				if(cur == prev)
					lyn = (j == 0) ? 1 : lyn;
				else if(cur > prev)
					lyn = j + 1;
				else if(cur < prev)
					break;
			}

			if(compare == 0)
			{
				for(size_t j = 0; j < i; j++)
				{
					if(tmp[j] == context.symmetry.period)
						base = (base + cd.points - context.symmetry.period) % cd.points;
				}

				backtrack(context.chord_len + 1, Symmetry(context.symmetry.points, new_period, true, (base + context.chord_len) % cd.points));
				return;
			}

			i += j - j % lyn;
		}
	}

	return backtrack(context.chord_len + 1, Symmetry(context.symmetry.points, new_period, false, 0));
}

void ChordGenerator::backtrack(const size_t chord_len, const Symmetry & symmetry)
{
	if(cd.isFull())
	{
		_total++;
		print(cd, symmetry);
		return;
	}

	if(symmetry.isTrivial() || chord_len >= cd.chords)
	{
		matchRest(chord_len, symmetry);
		return;
	}

	PlaceChordsContext context(chord_len, symmetry);
	placeChords(context, 0, 1, 0, 0);
}

void ChordGenerator::placeChords(PlaceChordsContext & context, const size_t pos, const size_t lyn, const size_t chunk, const size_t lower)
{
	if(chunk == context.symmetry.chunks())
	{
		if(pos % lyn == 0)
			checkSymmetry(context, pos, (cd.points * lyn) / pos);
		return;
	}

	const size_t lyndon_prev = (pos == 0) ? 0 : context.code[pos - lyn];
	const size_t bottom = (lyndon_prev > lower) ? lyndon_prev : lower;

	for(size_t j = bottom; j <= context.symmetry.period; j++)
	{
		context.code[pos] = j;
		const size_t next_lyn = (j == lyndon_prev) ? lyn : (pos + 1);

		if(j < context.symmetry.period)
		{
			size_t u = chunk * context.symmetry.period + j;
			size_t v = (u + context.chord_len) % cd.points;

			if(cd.isEmpty(u) && cd.isEmpty(v) && checkChord(cd, context.chord_len, u, v))
			{
				cd.addChord(u, v);
				placeChords(context, pos + 1, next_lyn, chunk, j + 1);
				cd.cancellChord();
			}
		}
		else
			placeChords(context, pos + 1, next_lyn, chunk + 1, 0);
	}
}

void ChordGenerator::matchRest(size_t min_chord, const Symmetry & symmetry)
{
	if(cd.isFull())
	{
		_total++;
		print(cd, symmetry);
		return;
	}

	size_t s = cd.firstEmpty();
	for(size_t e = cd.nextEmpty(s); e < cd.points; e = cd.nextEmpty(e))
	{
		if(cd.points + s - e < min_chord)
			break;

		if(e - s >= min_chord && checkChord(cd, e - s, s, e))
		{
			cd.addChord(s, e);
			matchRest(min_chord, symmetry);
			cd.cancellChord();
		}
	}
}
