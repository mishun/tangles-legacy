#include <iostream>
#include <vector>
#include <ChordDiagrams/ChordGenerator.h>


class StarGenerator : public ChordDiagrams::ChordGenerator
{
protected:
	std::vector<unsigned long long> by_genus;

public:
	StarGenerator(const size_t _n)
		: ChordGenerator(_n)
		, by_genus(chords, 0)
	{
	}

protected:
	virtual void print(const ChordDiagrams::ChordDiagram &, const Symmetry &);
	virtual bool checkChord(const ChordDiagrams::ChordDiagram &, size_t, size_t, size_t) const;
	virtual void init();
	virtual void done();
};

void StarGenerator::print(const ChordDiagrams::ChordDiagram & cd, const Symmetry &)
{
	assert(cd.isFull());
	by_genus[cd.genus()]++;
}

bool StarGenerator::checkChord(const ChordDiagrams::ChordDiagram & cd, const size_t len, const size_t u, const size_t v) const
{
	if(len == 1 || len == cd.points - 1)
		return false;

	//if((len & 1) == 0)
	//	return false;

	/*const size_t n = (u + 1) % cd.points;
	const size_t p = (u + cd.points - 1) % cd.points;

	if(!cd.isEmpty(n) && cd[n] == len - 2)
		return false;

	if(!cd.isEmpty(p) && cd[p] == len + 2)
		return false;*/

	return true;
}

void StarGenerator::init()
{
	by_genus.assign(chords, 0);
}

void StarGenerator::done()
{
	for(size_t i = 0; i < chords; i++)
		std::cout << by_genus[i] << " ";
	std::cout << "\n";
}


int main()
{
	for(size_t i = 1; i <= 10; i++)
	{
		StarGenerator fc(i);
		fc.generate();
	}
}
