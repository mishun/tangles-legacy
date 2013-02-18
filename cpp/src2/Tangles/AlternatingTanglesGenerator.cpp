#include <ctime>
#include <iostream>
#include <set>
#include "AlternatingTanglesGenerator.h"

using namespace Tangles;

AlternatingTanglesGenerator::AlternatingTanglesGenerator(size_t mv)
	: max_v(mv)
	, _total(0)
	, _pregenerated(0)
	, tangle_id(0)
	, subtangles(new std::vector<SubTangle *>[mv + 1])
{
}

AlternatingTanglesGenerator::~AlternatingTanglesGenerator()
{
	for(size_t i = 0; i <= max_v; i++)
		for(size_t j = 0; j < subtangles[i].size(); j++)
			delete subtangles[i] [j];

	delete[] subtangles;
}

void AlternatingTanglesGenerator::generate()
{
	_total = 0;
	_pregenerated = 0;
	init();

	const double begin_time = clock();

	{
		SubTangle * loner = new SubTangle(1, D4Group::SubGroup::D4, false, false);
		loner->tg = new TangleGraph();
		subtangles[1].push_back(loner);
		tangles.push_back(AlternatingTangle::create(*loner));
	}

	for(tangle_id = 0; tangle_id < tangles.size(); tangle_id++)
	{
		const AlternatingTangle & tangle = *tangles[tangle_id];
		for(size_t cross = 1; tangle.crossings() + cross <= max_v; cross++)
			for(size_t st = 0; st < subtangles[cross].size(); st++)
			{
				const SubTangle & subtangle = *subtangles[cross] [st];
				apply(tangle, subtangle);
			}
	}

	{
		double pregen_time = (clock() - begin_time) / CLOCKS_PER_SEC;
		std::cerr << "AlternatingTanglesGenerator::generate(): " << pregenerated() << " pre-generated";
		std::cerr << " (" << pregen_time << "s, " << pregenerated() / pregen_time << "t/s)\n";
	}

	for(size_t cross = 1; cross <= max_v; cross++)
		for(size_t st = 0; st < subtangles[cross].size(); st++)
		{
			const SubTangle & subtangle = *subtangles[cross] [st];
			AlternatingTangle * tangle = AlternatingTangle::create(subtangle);
			backtrack(*tangle);
			tangle->destroy();
		}

	{
		const double total_time = (clock() - begin_time) / CLOCKS_PER_SEC;
		std::cerr << "AlternatingTanglesGenerator::generate(): " << total() << " tangles total";
		std::cerr << " (" << total_time << "s, " << total() / total_time << "t/s)\n";
	}

	done();
}

unsigned long long AlternatingTanglesGenerator::total() const
{
	return _total;
}

unsigned long long AlternatingTanglesGenerator::pregenerated() const
{
	return _pregenerated;
}

void AlternatingTanglesGenerator::init()
{
}

void AlternatingTanglesGenerator::print(const AlternatingTangle &)
{
}

void AlternatingTanglesGenerator::done()
{
}

void AlternatingTanglesGenerator::addSubTangle(const AlternatingTangle & tangle)
{
	_pregenerated++;

	SubTangle * st = new SubTangle(tangle.crossings()
		, tangle.getSymmetryGroup()
		, tangle.axis(D4Group::identity)
		, tangle.axis(D4Group(1, false))
		);

	st->tg = new TangleGraph(tangle.getGraph());

	tangles.push_back(AlternatingTangle::create(*st));

	subtangles[tangle.crossings()].push_back(st);
	for(unsigned i = 0; i < tangle_id; i++)
		apply(*tangles[i], *st);
}

void AlternatingTanglesGenerator::apply(const AlternatingTangle & tangle, const SubTangle & subtangle)
{
	if(tangle.crossings() + subtangle.crossings() > max_v)
		return;

	for(size_t gl = 3; gl >= 1; gl--)
	{
		if(tangle.crossings() + subtangle.crossings() + (tangle.legs() + 4 - 2 * gl) / 2 - 2 > max_v)
			continue;

		std::set<RootCode> code_list;
		std::set<size_t> subtangle_codes;
		for(size_t rot = 0; rot < 8; rot++)
		{
			D4Group orientation(rot >> 1, rot & 1);

			size_t orbit = subtangle.symmetry.factorSet(orientation);
			if(subtangle_codes.count(orbit) > 0)
				continue;
			subtangle_codes.insert(orbit);

			for(size_t pos = 0; pos < tangle.legs(); pos++)
			{
				AlternatingTangle * next = tangle.glue(subtangle, orientation, pos, gl);
				if(next == 0)
					continue;

				const RootCode & code = next->getCode();
				if(code_list.count(code) == 0)
				{
					code_list.insert(code);
					if(next->legs() == 4)
						addSubTangle(*next);
					tangles.push_back(next);
				}
				else
					next->destroy();
			}
		}
	}
}

void AlternatingTanglesGenerator::backtrack(const AlternatingTangle & tangle)
{
	_total++;
	print(tangle);

	for(size_t cross = 1; tangle.crossings() + cross <= max_v; cross++)
		for(size_t st = 0; st < subtangles[cross].size(); st++)
		{
			const SubTangle & subtangle = *subtangles[cross] [st];
			for(size_t gl = 3; gl >= 1; gl--)
			{
				if(tangle.legs() + 4 - 2 * gl <= 4)
					continue;

				std::set<RootCode> code_list;
				std::set<size_t> subtangle_codes;

				for(size_t rot = 0; rot < 8; rot++)
				{
					D4Group orientation(rot >> 1, rot & 1);

					size_t orbit = subtangle.symmetry.factorSet(orientation);
					if(subtangle_codes.count(orbit) > 0)
						continue;
					subtangle_codes.insert(orbit);

					for(size_t pos = 0; pos < tangle.legs(); pos++)
					{
						AlternatingTangle * next = tangle.glue(subtangle, orientation, pos, gl);
						if(next == 0)
							continue;

						const RootCode & code = next->getCode();
						if(code_list.count(code) == 0)
						{
							code_list.insert(code);
							backtrack(*next);
						}

						next->destroy();
					}
				}
			}
		}
}
