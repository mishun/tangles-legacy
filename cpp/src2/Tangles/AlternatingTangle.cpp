#include <iostream>
#include <sstream>
#include "AlternatingTangle.h"

using namespace Tangles;

std::string AlternatingTangle::toString() const
{
	std::ostringstream out;
	out << "Tangle {\n";
	out << "\tcrossings = " << crossings() << "\n";
	out << "\tsubtangles = " << subtangles() << "\n";
	out << "\tlegs = " << legs() << "\n";
	out << "\tsymmetry group = " << getSymmetryGroup().toString() << "\n";
	out << "}\n";
	return out.str();
}

bool AlternatingTangle::isWeakIrreducible() const
{
	TangleGraph graph = getGraph();
	for(size_t i = 0; i < legs(); i++)
	{
		size_t a = graph.border(i) >> 2;
		size_t b = graph.border((i + 1) % legs()) >> 2;
		if(a == b)
			return false;
	}

	if(graph.getCutpoints() != 0)
		return false;

	return true;
}

bool AlternatingTangle::preCheck(const SubTangle & sub, D4Group orientation, size_t glue_pos, size_t glue_edges) const
{
	if(!TangleTemplate::preCheck(sub, orientation, glue_pos, glue_edges))
		return false;

	if(legs() == 4 && glue_edges == 2)
	{
		size_t opposite_position = (glue_position + 2) & 3;
		bool glue_loner = (vertex(border(glue_pos) >> 2).crossings() == 1);
		bool opposite_loner = (vertex(border(opposite_position) >> 2).crossings() == 1);

		if(sub.crossings() == 1)
		{
			if(glue_loner)
				return true;

			if(opposite_loner && !glue_loner)
				return false;

			return true;
		}
		else
		{
			if(!glue_loner && !opposite_loner)
				return true;

			if(!opposite_loner && glue_loner)
				return false;

			return false; // cutoff by root-code
		}
	}

	return true;
}

bool AlternatingTangle::postCheck() const
{
	if(!TangleTemplate::postCheck())
		return false;

	if(legs() == 4 && ancestor().legs() == 4)
	{
		size_t opposite_position = (glue_position + 2) & 3;
		bool glue_loner = (ancestor().vertex(ancestor().border(glue_position) >> 2).crossings() == 1);
		bool opposite_loner = (ancestor().vertex(ancestor().border(opposite_position) >> 2).crossings() == 1);

		if(vertex(last()).crossings() == 1)
		{
			if(glue_loner)
				return true;

			if(opposite_loner && !glue_loner)
				return false;

			AlternatingTangle flyp(ancestor(), vertex(last()).getSubTangle(), vertex(last()).getOrientation(), opposite_position, glue_edges);

			if(flyp.getCode() < getCode())
				return false;
			else
				return true;
		}
		else
		{
			if(!glue_loner && !opposite_loner)
				return true;

			if(!opposite_loner && glue_loner)
				return false;

			std::cerr << "Fucking case!!!\n";
			assert(false);
			return true;
		}
	}

	return true;
}

const D4Group::SubGroup & AlternatingTangle::calcSymmetryGroup() const
{
	const D4Group::SubGroup & isotopy_subgroup = TangleTemplate::calcSymmetryGroup();

	if(legs() == 4 && (axis(D4Group::I) || axis(D4Group::C)))
	{
		size_t loners = 0;
		for(size_t i = 1; i <= subtangles(); i++)
			if(vertex(i).crossings() == 1)
				loners++;

		if(loners == 0)
			return isotopy_subgroup;

		std::vector<size_t> orbit;
		{
			size_t v;
			if(axis(D4Group::I))
				v = border(0);
			else
				v = border(1);

			size_t s = 0, l = 0;
			while(v != 0)
			{
				if(vertex(v >> 2).crossings() > 1)
				{
					orbit.push_back(v);
					s++;
					assert(l == 0 || l == loners);
				}
				else
				{
					l++;
					assert(s == 0 || s == subtangles() - loners);
				}
				v = neighbour(v >> 2, (v + 3) & 3);
			}
		}

		bool mir = true, rot = true;
		for(size_t i = 0; i < orbit.size(); i++)
		{
			size_t a = orbit[i], b = orbit[orbit.size() - 1 - i];

			if(vertex(a >> 2).subtangleCode() != vertex(b >> 2).subtangleCode())
			{
				mir = rot = false;
				break;
			}

			size_t dir_code = vertex(a >> 2).orientationCode(D4Group(a & 3, false));
			size_t mir_code = vertex(b >> 2).orientationCode(D4Group((b + 3) & 3, true));
			size_t rot_code = vertex(b >> 2).orientationCode(D4Group((b + 2) & 3, false));
			mir = mir && dir_code == mir_code;
			rot = rot && dir_code == rot_code;
		}

		if(loners & 1)
			std::swap(mir, rot);

		D4Group flype_symmetry = D4Group::I;
		if(rot)
			flype_symmetry = D4Group::CC;
		else if(mir)
		{
			if(axis(D4Group::I))
				flype_symmetry = D4Group::ECCC;
			else
				flype_symmetry = D4Group::EC;
		}

		return isotopy_subgroup.addSymmetry(flype_symmetry);
	}
	else
		return isotopy_subgroup;
}
