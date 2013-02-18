#include <cassert>
#include <sstream>
#include "D4Group.h"

using namespace Tangles;

const D4Group D4Group::identity(0, false);
const D4Group D4Group::I   (0, false);
const D4Group D4Group::E   (0, true);
const D4Group D4Group::C   (1, false);
const D4Group D4Group::EC  (1, true);
const D4Group D4Group::CC  (2, false);
const D4Group D4Group::ECC (2, true);
const D4Group D4Group::CCC (3, false);
const D4Group D4Group::ECCC(3, true);


std::string D4Group::toString() const
{
	if(*this == identity)
		return "I";

	std::ostringstream out;
	if(mirror())
		out << 'E';
	for(size_t i = 0; i < rotation(); i++)
		out << 'C';
	return out.str();
}


// D4 = {I, E, C, EC, CC, ECC, CCC, ECCC}
// D4 / D4 = { {I, E, C, EC, CC, ECC, CCC, ECCC} }
const D4Group::SubGroup D4Group::SubGroup::D4(0, 0, 0, 0, 0, 0, 0, 0, "D4");

// C4 = {I, C, CC, CCC}
// D4 / C4 = { {I, C, CC, CCC}, {E, EC, ECC, ECCC} }
const D4Group::SubGroup D4Group::SubGroup::C4(0, 1, 0, 1, 0, 1, 0, 1, "C4");

// GS = {I, CC, EC, ECCC}
// D4 / GS = { {I, CC, EC, ECCC}, {C, CCC, E, ECC} }
const D4Group::SubGroup D4Group::SubGroup::GS(0, 1, 1, 0, 0, 1, 1, 0, "GS");

// DS = {I, CC, E, ECC}
// D4 / DS = { {I, CC, E, ECC}, {C, CCC, EC, ECCC} }
const D4Group::SubGroup D4Group::SubGroup::DS(0, 0, 1, 1, 0, 0, 1, 1, "DS");

// C2 = {I, CC}
// D4 / C2 = { {I, CC}, {C, CCC}, {E, ECC}, {EC, ECCC} }
const D4Group::SubGroup D4Group::SubGroup::C2(0, 2, 1, 3, 0, 2, 1, 3, "C2");

// ES = {I, E}
// D4 / ES = { {I, E}, {C, ECCC}, {CC, ECC}, {CCC, EC} }
const D4Group::SubGroup D4Group::SubGroup::ES(0, 0, 1, 3, 2, 2, 3, 1, "ES");

// ECS = {I, EC}
// D4 / ECS = { {I, EC}, {C, E}, {CC, ECCC}, {CCC, ECC} }
const D4Group::SubGroup D4Group::SubGroup::ECS(0, 1, 1, 0, 2, 3, 3, 2, "ECS");

// EC2S = {I, ECC}
// D4 / EC2S = { {I, ECC}, {CC, E}, {C, EC}, {CCC, ECCC} }
const D4Group::SubGroup D4Group::SubGroup::EC2S(0, 1, 2, 2, 1, 0, 3, 3, "EC2S");

// EC3S = {I, ECCC}
// D4 / EC3S = { {I, ECCC}, {E, CCC}, {C, ECC}, {CC, EC} }
const D4Group::SubGroup D4Group::SubGroup::EC3S(0, 1, 2, 3, 3, 2, 1, 0, "EC3S");

// ID = {I}
// D4 / ID = { {I}, {E}, {C}, {EC}, {CC}, {ECC}, {CCC}, {ECCC} }
const D4Group::SubGroup D4Group::SubGroup::ID(0, 1, 2, 3, 4, 5, 6, 7, "ID");

D4Group::SubGroup::SubGroup(size_t f0, size_t f1, size_t f2, size_t f3, size_t f4, size_t f5, size_t f6, size_t f7, const char * nm)
	: _name(nm)
{
	factor[0] = f0;
	factor[1] = f1;
	factor[2] = f2;
	factor[3] = f3;
	factor[4] = f4;
	factor[5] = f5;
	factor[6] = f6;
	factor[7] = f7;

	_size = 0;
	for(size_t i = 0; i < 8; i++)
		if(factorSet(D4Group(i >> 1, i & 1)) == 0)
			_size++;

	assert(checkCorrectness());
}

bool D4Group::SubGroup::checkCorrectness() const
{
	if(8 % size() != 0)
		return false;

	size_t classes = 8 / size();

	if(factorSet(D4Group(0, false)) != 0)
		return false;

	for(size_t c = 0; c < classes; c++)
	{
		size_t counter = 0;
		for(size_t i = 0; i < 8; i++)
		{
			size_t f = factorSet(D4Group(i >> 1, i & 1));
			if(f >= classes)
				return false;
			if(f == c)
				counter++;
		}

		if(counter != size())
			return false;
	}

	for(size_t i = 0; i < 8; i++)
	{
		D4Group h(i >> 1, i & 1);
		if(factorSet(h) != 0)
			continue;

		for(size_t j = 0; j < 8; j++)
		{
			D4Group g(j >> 1, j & 1);
			if(factorSet(g) != factorSet(g * h))
				return false;
		}
	}

	return true;
}

const D4Group::SubGroup & D4Group::SubGroup::addSymmetry(D4Group g) const
{
	if(factorSet(g) == 0)
		return *this;

	if(this == &C4 || this == &GS || this == &DS)
		return D4;

	if(this == &C2)
	{
		if(!g.mirror())
			return C4;
		else if(g.rotation() & 1)
			return GS;
		else
			return DS;
	}

	if(this == &ES)
	{
		if(g == D4Group::CC || g == D4Group::ECC)
			return DS;
		else
			return D4;
	}

	if(this == &ECS)
	{
		if(g == D4Group::CC || g == D4Group::ECCC)
			return GS;
		else
			return D4;
	}

	if(this == &EC2S)
	{
		if(g == D4Group::CC || g == D4Group::E)
			return DS;
		else
			return D4;
	}

	if(this == &EC3S)
	{
		if(g == D4Group::CC || g == D4Group::EC)
			return GS;
		else
			return D4;
	}

	assert(this == &ID);
	if(g == D4Group::CC)
		return C2;
	else if(g == D4Group::E)
		return ES;
	else if(g == D4Group::EC)
		return ECS;
	else if(g == D4Group::ECC)
		return EC2S;
	else if(g == D4Group::ECCC)
		return EC3S;
	else
		return C4;
}

std::string D4Group::SubGroup::toString() const
{
	std::ostringstream out;
	out << _name << " ({";
	size_t classes = 8 / size();
	for(size_t i = 0; i < classes; i++)
	{
		out << '{';
		size_t k = 0;
		for(size_t j = 0; j < 8; j++)
		{
			D4Group g(j >> 1, j & 1);
			if(factorSet(g) == i)
			{
				out << g.toString();
				k++;
				if(k < size())
					out << ", ";
			}
		}
		out << '}';
		if(i + 1 < classes)
			out << ", ";
	}
	out << "})";
	return out.str();
}
