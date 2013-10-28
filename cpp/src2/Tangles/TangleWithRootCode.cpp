#include <cstring>
#include <Util/Util.h>
#include "TangleWithRootCode.h"

using namespace Tangles;

bool TangleWithRootCode::preCheck(const SubTangle & sub, D4Group orientation, size_t glue_pos, size_t glue_edges) const
{
	if(legs() + 4 - 2 * glue_edges < 4)
		return false;

	for(size_t i = 0; i + 1 < glue_edges; i++)
	{
		size_t a = border((glue_pos + legs() - i) % legs());
		size_t b = border((glue_pos + legs() - i - 1) % legs());

		if(a == 0 || (a ^ b) > 3 || !sub.axis(D4Group(i, false) * orientation))
			continue;

		if(vertex(a >> 2).axis(D4Group(b & 3, false)))
			return false;
	}


/*	size_t root_mask = getCutpoints();
	{
		size_t border_mask = 0;
		for(size_t i = 0; i < legs(); i++)
			border_mask |= (1 << (border(i) >> 2));
		root_mask = border_mask & ~root_mask;
	}

	for(size_t i = 0; i < glue_edges; i++)
		root_mask &= ~(1 << (border((glue_pos + legs() - i) % legs()) >> 2));

	size_t root_subtangle = sub.code();
	for(size_t i = 1; i <= subtangles(); i++)
		if(root_mask & (1 << i))
		{
			size_t subtangle = vertex(i).subtangleCode();
			if(subtangle < root_subtangle)
				return false;
		}*/

	return true;
}

bool TangleWithRootCode::postCheck() const
{
	size_t root_mask = getCutpoints();

	if(root_mask & (1 << last())) // is not prime
		return false;

	{
		size_t border_mask = 0;
		for(size_t i = 0; i < legs(); i++)
			border_mask |= (1 << (border(i) >> 2));
		root_mask = border_mask & ~root_mask;
	}

	{
		size_t root_subtangle = vertex(last()).subtangleCode();
		for(size_t i = 1; i <= subtangles(); i++)
			if(root_mask & (1 << i))
			{
				size_t subtangle = vertex(i).subtangleCode();

				if(subtangle < root_subtangle)
					return false;

				if(subtangle > root_subtangle)
					root_mask &= ~(1 << i);
			}
	}

	const RootCode & rc = getCode();

	root_mask &= ~(1 << last());
	for(size_t i = 1; i <= subtangles(); i++)
		if(root_mask & (1 << i))
		{
			size_t j = 0;

			while(neighbour(i, j) == 0)
				j = (j + 1) & 3;

			while(neighbour(i, j) != 0)
				j = (j + 1) & 3;

			{
				int compareResult = compareRootCode(rc.getData(), i, j, 1);

				if(compareResult > 0)
					return false;

				if(compareResult == 0)
					symmetryOrder[0]++;
			}

			while(neighbour(i, j) == 0)
				j = (j + 1) & 3;
			j = (j - 1) & 3;

			{
				int compareResult = compareRootCode(rc.getData(), i, j, -1);

				if(compareResult > 0)
					return false;

				if(compareResult == 0)
					symmetryOrder[1]++;
			}
		}

	return true;
}

const D4Group::SubGroup & TangleWithRootCode::calcSymmetryGroup() const
{
	assert(legs() == 4);

	RootCode id = calcSymmetryCode(D4Group::identity);

	bool f = (id == calcSymmetryCode(D4Group::ECCC));
	bool g = (id == calcSymmetryCode(D4Group::ECC));

	if(id == calcSymmetryCode(D4Group(2, false)))
	{
		if(id == calcSymmetryCode(D4Group::C))
		{
			if(f)
				return D4Group::SubGroup::D4;
			else
				return D4Group::SubGroup::C4;
		}

		if(f)
			return D4Group::SubGroup::GS;

		if(g)
			return D4Group::SubGroup::DS;

		return D4Group::SubGroup::C2;
	}
	else
	{
		if(f)
			return D4Group::SubGroup::EC3S;

		if(g)
			return D4Group::SubGroup::EC2S;

		bool d = (id == calcSymmetryCode(D4Group::EC));
		if(d)
			return D4Group::SubGroup::ECS;

		bool e = (id == calcSymmetryCode(D4Group::E));
		if(e)
			return D4Group::SubGroup::ES;
	}
	return D4Group::SubGroup::ID;
}

RootCode TangleWithRootCode::calcCode() const
{
	Util::AutoArray<size_t> rcode(2 * subtangles(), 0xFFFFFFFF);

	symmetryOrder[0] = 1;
	symmetryOrder[1] = 0;

	tryRootCode(rcode.get(), last(), glue_edges, 1);

	int tryResult = tryRootCode(rcode.get(), last(), 3, -1);

	if(tryResult >= 0)
	{
		symmetryOrder[1] = 1;
		if(tryResult > 0)
			symmetryOrder[0] = 0;
	}

//	size_t a = vertex(neighbour(last(), 0) >> 2).subtangleCode();
//	size_t b = vertex(neighbour(last(), glue_edges - 1) >> 2).subtangleCode();

//	if(a <= b)
//		tryRootCode(&rcode[0], last(), glue_edges, 1);

//	if(b <= a)
//		tryRootCode(&rcode[0], last(), 3, -1);

	return RootCode(2 * subtangles(), rcode.get());
}

size_t TangleWithRootCode::calcCutpoints() const
{
	if(glue_edges == 1)
	{
		size_t cutpoints = ancestor().getCutpoints();
		if(ancestor().subtangles() > 1)
			cutpoints |= (1 << (ancestor().border(glue_position) >> 2));
		return cutpoints;
	}

	size_t timer = 1, cutpoints = 0;
	Util::AutoArray<size_t> tin(subtangles() + 1, 0);

	tin.get()[last()] = timer++;
	std::pair<size_t, bool> out = dfsCP(neighbour(last(), 0) >> 2, timer, cutpoints, tin.get());

	if(!out.second)
		return cutpoints | (1 << last());
	else
		return cutpoints;
}

size_t TangleWithRootCode::fastCode(const size_t start) const
{
	size_t id = 0, free_id = 1, code = 0;
	for(size_t i = 0; i < 4; i++)
	{
		code <<= 2;

		size_t cur = neighbour(start, i);

		if(cur != 0)
		{
			size_t cid = (id >> (cur + cur)) & 3;
			if(cid == 0)
			{
				id |= free_id << (cur + cur);
				cid = free_id++;
			}
			code |= cid;
		}
	}

	return vertex_table[code] [0];
}

int TangleWithRootCode::tryRootCode(size_t rcode[], const size_t start, const size_t e, const int dir) const
{
	assert(1 <= start && start <= subtangles());
	assert(e < 4);
	assert(dir == 1 || dir == -1);
	assert(neighbour(start, 0) == 0 || neighbour(start, 1) == 0 || neighbour(start, 2) == 0 || neighbour(start, 3) == 0);

	Util::AutoArray<size_t> memory(2 * subtangles() + 1);
	size_t * queue = memory.get();
	size_t * id = memory.get() + subtangles();

	memset(id, 0, sizeof(size_t[subtangles() + 1]));
	id[start] = 1;
	size_t head = 0, tail = 0;
	queue[tail++] = (start << 2) | e;

	size_t free_id = 2;
	bool better = false;
	for( ; head < subtangles(); head++)
	{
		size_t cur = queue[head];
		size_t ver = cur >> 2;
		size_t base = cur & 3;

		size_t vcode = 0, i = base;
		do
		{
			size_t neigh = neighbour(ver, i) >> 2;
			vcode <<= max_log_size;
			if(neigh != 0)
			{
				if(id[neigh] == 0)
				{
					id[neigh] = free_id++;
					queue[tail++] = neighbour(ver, i);
				}
				vcode |= id[neigh];
			}
			i = (i + dir) & 3;
		} while(i != base);

		vcode = (vcode << 3) | vertex(ver).orientationCode(D4Group(base, dir < 0));
		size_t subtangle_code = vertex(ver).subtangleCode();

		if(better || subtangle_code < rcode[2 * head])
		{
			rcode[2 * head] = subtangle_code;
			better = true;
		}
		else if(subtangle_code > rcode[2 * head])
			return -1;

		if(better || vcode < rcode[2 * head + 1])
		{
			rcode[2 * head + 1] = vcode;
			better = true;
		}
		else if(vcode > rcode[2 * head + 1])
			return -1;
	}

	if(better)
		return 1;
	return 0;
}

int TangleWithRootCode::compareRootCode(const size_t rcode[], const size_t start, const size_t e, const int dir) const
{
	assert(1 <= start && start <= subtangles());
	assert(e < 4);
	assert(dir == 1 || dir == -1);
	assert(neighbour(start, 0) == 0 || neighbour(start, 1) == 0 || neighbour(start, 2) == 0 || neighbour(start, 3) == 0);

	Util::AutoArray<size_t> memory(2 * subtangles() + 1);
	size_t * queue = memory.get();
	size_t * id = memory.get() + subtangles();

	std::memset(id, 0, sizeof(size_t[subtangles() + 1]));
	id[start] = 1;
	size_t head = 0, tail = 0;
	queue[tail++] = (start << 2) | e;

	size_t free_id = 2;
	for( ; head < subtangles(); head++)
	{
		size_t cur = queue[head];
		size_t ver = cur >> 2;
		size_t base = cur & 3;

		size_t vcode = 0, i = base;
		do
		{
			size_t neigh = neighbour(ver, i) >> 2;
			vcode <<= max_log_size;
			if(neigh != 0)
			{
				if(id[neigh] == 0)
				{
					id[neigh] = free_id++;
					queue[tail++] = neighbour(ver, i);
				}
				vcode |= id[neigh];
			}
			i = (i + dir) & 3;
		} while(i != base);

		vcode = (vcode << 3) | vertex(ver).orientationCode(D4Group(base, dir < 0));
		size_t subtangle_code = vertex(ver).subtangleCode();

		if(subtangle_code < rcode[2 * head])
			return 1;
		if(subtangle_code > rcode[2 * head])
			return -1;

		if(vcode < rcode[2 * head + 1])
			return 1;
		if(vcode > rcode[2 * head + 1])
			return -1;
	}
	return 0;
}

RootCode TangleWithRootCode::calcSymmetryCode(D4Group g) const
{
	assert(legs() == 4);

	Util::AutoArray<size_t> rcode(2 * subtangles(), 0xFFFFFFFF);
	size_t v = border(g.rotation());
	tryRootCode(rcode.get(), v >> 2, v & 3, g.mirror() ? -1 : 1);
	return RootCode(2 * subtangles(), rcode.get());
}

std::pair<size_t, bool> TangleWithRootCode::dfsCP(size_t v, size_t & timer, size_t & cutpoint, size_t tin[]) const
{
	size_t fup = tin[v] = timer++;
	bool border_accessible = false;

	for(size_t i = 0; i < 4; i++)
	{
		size_t to = neighbour(v, i) >> 2;
		if(to == 0)
		{
			border_accessible = true;
			continue;
		}

		if(tin[to] > 0)
		{
			if(tin[to] < fup)
				fup = tin[to];
		}
		else
		{
			std::pair<size_t, bool> fup_to = dfsCP(to, timer, cutpoint, tin);

			if(fup_to.first < fup)
				fup = fup_to.first;

			if(fup_to.first > tin[v])
				cutpoint |= (1 << v);
			else
			{
				if(fup_to.first >= tin[v])
					cutpoint |= (1 << v);
				border_accessible = border_accessible || fup_to.second;
			}
		}
	}

	return std::pair<size_t, bool>(fup, border_accessible);
}

const size_t TangleWithRootCode::vertex_table[256] [2] = {
	{0x00000000, 0x00},	// 0
	{0x00000002, 0x12},	// 1
	{0x00000000, 0x00},	// 2
	{0x00000000, 0x00},	// 3
	{0x00000002, 0x84},	// 4
	{0x00000202, 0x06},	// 5
	{0x00000203, 0x06},	// 6
	{0x00000000, 0x00},	// 7
	{0x00000000, 0x00},	// 8
	{0x00000000, 0x00},	// 9
	{0x00000000, 0x00},	// 10
	{0x00000000, 0x00},	// 11
	{0x00000000, 0x00},	// 12
	{0x00000000, 0x00},	// 13
	{0x00000000, 0x00},	// 14
	{0x00000000, 0x00},	// 15
	{0x00000002, 0x21},	// 16
	{0x00020002, 0x33},	// 17
	{0x00020003, 0x33},	// 18
	{0x00000000, 0x00},	// 19
	{0x00000202, 0x81},	// 20
	{0x00020202, 0x03},	// 21
	{0x00020203, 0x02},	// 22
	{0x00000000, 0x00},	// 23
	{0x00000203, 0x81},	// 24
	{0x00020302, 0x03},	// 25
	{0x00020203, 0x01},	// 26
	{0x00020304, 0x03},	// 27
	{0x00000000, 0x00},	// 28
	{0x00000000, 0x00},	// 29
	{0x00000000, 0x00},	// 30
	{0x00000000, 0x00},	// 31
	{0x00000000, 0x00},	// 32
	{0x00000000, 0x00},	// 33
	{0x00000000, 0x00},	// 34
	{0x00000000, 0x00},	// 35
	{0x00000000, 0x00},	// 36
	{0x00000000, 0x00},	// 37
	{0x00000000, 0x00},	// 38
	{0x00000000, 0x00},	// 39
	{0x00000000, 0x00},	// 40
	{0x00000000, 0x00},	// 41
	{0x00000000, 0x00},	// 42
	{0x00000000, 0x00},	// 43
	{0x00000000, 0x00},	// 44
	{0x00000000, 0x00},	// 45
	{0x00000000, 0x00},	// 46
	{0x00000000, 0x00},	// 47
	{0x00000000, 0x00},	// 48
	{0x00000000, 0x00},	// 49
	{0x00000000, 0x00},	// 50
	{0x00000000, 0x00},	// 51
	{0x00000000, 0x00},	// 52
	{0x00000000, 0x00},	// 53
	{0x00000000, 0x00},	// 54
	{0x00000000, 0x00},	// 55
	{0x00000000, 0x00},	// 56
	{0x00000000, 0x00},	// 57
	{0x00000000, 0x00},	// 58
	{0x00000000, 0x00},	// 59
	{0x00000000, 0x00},	// 60
	{0x00000000, 0x00},	// 61
	{0x00000000, 0x00},	// 62
	{0x00000000, 0x00},	// 63
	{0x00000002, 0x48},	// 64
	{0x00000202, 0x18},	// 65
	{0x00000203, 0x18},	// 66
	{0x00000000, 0x00},	// 67
	{0x00020002, 0xCC},	// 68
	{0x00020202, 0x0C},	// 69
	{0x00020302, 0x0C},	// 70
	{0x00000000, 0x00},	// 71
	{0x00020003, 0xCC},	// 72
	{0x00020203, 0x04},	// 73
	{0x00020203, 0x08},	// 74
	{0x00020304, 0x0C},	// 75
	{0x00000000, 0x00},	// 76
	{0x00000000, 0x00},	// 77
	{0x00000000, 0x00},	// 78
	{0x00000000, 0x00},	// 79
	{0x00000202, 0x60},	// 80
	{0x00020202, 0x30},	// 81
	{0x00020203, 0x10},	// 82
	{0x00000000, 0x00},	// 83
	{0x00020202, 0xC0},	// 84
	{0x02020202, 0xFF},	// 85
	{0x02020203, 0x12},	// 86
	{0x00000000, 0x00},	// 87
	{0x00020203, 0x80},	// 88
	{0x02020203, 0x84},	// 89
	{0x02020303, 0x66},	// 90
	{0x02020304, 0x06},	// 91
	{0x00000000, 0x00},	// 92
	{0x00000000, 0x00},	// 93
	{0x00000000, 0x00},	// 94
	{0x00000000, 0x00},	// 95
	{0x00000203, 0x60},	// 96
	{0x00020203, 0x20},	// 97
	{0x00020302, 0x30},	// 98
	{0x00020304, 0x30},	// 99
	{0x00020302, 0xC0},	// 100
	{0x02020203, 0x21},	// 101
	{0x02030203, 0xFF},	// 102
	{0x02030204, 0x33},	// 103
	{0x00020203, 0x40},	// 104
	{0x02020303, 0x99},	// 105
	{0x02020203, 0x48},	// 106
	{0x02020304, 0x18},	// 107
	{0x00020304, 0xC0},	// 108
	{0x02020304, 0x81},	// 109
	{0x02030204, 0xCC},	// 110
	{0x02020304, 0x60},	// 111
	{0x00000000, 0x00},	// 112
	{0x00000000, 0x00},	// 113
	{0x00000000, 0x00},	// 114
	{0x00000000, 0x00},	// 115
	{0x00000000, 0x00},	// 116
	{0x00000000, 0x00},	// 117
	{0x00000000, 0x00},	// 118
	{0x00000000, 0x00},	// 119
	{0x00000000, 0x00},	// 120
	{0x00000000, 0x00},	// 121
	{0x00000000, 0x00},	// 122
	{0x00000000, 0x00},	// 123
	{0x00000000, 0x00},	// 124
	{0x00000000, 0x00},	// 125
	{0x00000000, 0x00},	// 126
	{0x00000000, 0x00},	// 127
	{0x00000000, 0x00},	// 128
	{0x00000000, 0x00},	// 129
	{0x00000000, 0x00},	// 130
	{0x00000000, 0x00},	// 131
	{0x00000000, 0x00},	// 132
	{0x00000000, 0x00},	// 133
	{0x00000000, 0x00},	// 134
	{0x00000000, 0x00},	// 135
	{0x00000000, 0x00},	// 136
	{0x00000000, 0x00},	// 137
	{0x00000000, 0x00},	// 138
	{0x00000000, 0x00},	// 139
	{0x00000000, 0x00},	// 140
	{0x00000000, 0x00},	// 141
	{0x00000000, 0x00},	// 142
	{0x00000000, 0x00},	// 143
	{0x00000000, 0x00},	// 144
	{0x00000000, 0x00},	// 145
	{0x00000000, 0x00},	// 146
	{0x00000000, 0x00},	// 147
	{0x00000000, 0x00},	// 148
	{0x00000000, 0x00},	// 149
	{0x00000000, 0x00},	// 150
	{0x00000000, 0x00},	// 151
	{0x00000000, 0x00},	// 152
	{0x00000000, 0x00},	// 153
	{0x00000000, 0x00},	// 154
	{0x00000000, 0x00},	// 155
	{0x00000000, 0x00},	// 156
	{0x00000000, 0x00},	// 157
	{0x00000000, 0x00},	// 158
	{0x00000000, 0x00},	// 159
	{0x00000000, 0x00},	// 160
	{0x00000000, 0x00},	// 161
	{0x00000000, 0x00},	// 162
	{0x00000000, 0x00},	// 163
	{0x00000000, 0x00},	// 164
	{0x00000000, 0x00},	// 165
	{0x00000000, 0x00},	// 166
	{0x00000000, 0x00},	// 167
	{0x00000000, 0x00},	// 168
	{0x00000000, 0x00},	// 169
	{0x00000000, 0x00},	// 170
	{0x00000000, 0x00},	// 171
	{0x00000000, 0x00},	// 172
	{0x00000000, 0x00},	// 173
	{0x00000000, 0x00},	// 174
	{0x00000000, 0x00},	// 175
	{0x00000000, 0x00},	// 176
	{0x00000000, 0x00},	// 177
	{0x00000000, 0x00},	// 178
	{0x00000000, 0x00},	// 179
	{0x00000000, 0x00},	// 180
	{0x00000000, 0x00},	// 181
	{0x00000000, 0x00},	// 182
	{0x00000000, 0x00},	// 183
	{0x00000000, 0x00},	// 184
	{0x00000000, 0x00},	// 185
	{0x00000000, 0x00},	// 186
	{0x00000000, 0x00},	// 187
	{0x00000000, 0x00},	// 188
	{0x00000000, 0x00},	// 189
	{0x00000000, 0x00},	// 190
	{0x00000000, 0x00},	// 191
	{0x00000000, 0x00},	// 192
	{0x00000000, 0x00},	// 193
	{0x00000000, 0x00},	// 194
	{0x00000000, 0x00},	// 195
	{0x00000000, 0x00},	// 196
	{0x00000000, 0x00},	// 197
	{0x00000000, 0x00},	// 198
	{0x00000000, 0x00},	// 199
	{0x00000000, 0x00},	// 200
	{0x00000000, 0x00},	// 201
	{0x00000000, 0x00},	// 202
	{0x00000000, 0x00},	// 203
	{0x00000000, 0x00},	// 204
	{0x00000000, 0x00},	// 205
	{0x00000000, 0x00},	// 206
	{0x00000000, 0x00},	// 207
	{0x00000000, 0x00},	// 208
	{0x00000000, 0x00},	// 209
	{0x00000000, 0x00},	// 210
	{0x00000000, 0x00},	// 211
	{0x00000000, 0x00},	// 212
	{0x00000000, 0x00},	// 213
	{0x00000000, 0x00},	// 214
	{0x00000000, 0x00},	// 215
	{0x00000000, 0x00},	// 216
	{0x00000000, 0x00},	// 217
	{0x00000000, 0x00},	// 218
	{0x00000000, 0x00},	// 219
	{0x00000000, 0x00},	// 220
	{0x00000000, 0x00},	// 221
	{0x00000000, 0x00},	// 222
	{0x00000000, 0x00},	// 223
	{0x00000000, 0x00},	// 224
	{0x00000000, 0x00},	// 225
	{0x00000000, 0x00},	// 226
	{0x00000000, 0x00},	// 227
	{0x00000000, 0x00},	// 228
	{0x00000000, 0x00},	// 229
	{0x00000000, 0x00},	// 230
	{0x00000000, 0x00},	// 231
	{0x00000000, 0x00},	// 232
	{0x00000000, 0x00},	// 233
	{0x00000000, 0x00},	// 234
	{0x00000000, 0x00},	// 235
	{0x00000000, 0x00},	// 236
	{0x00000000, 0x00},	// 237
	{0x00000000, 0x00},	// 238
	{0x00000000, 0x00},	// 239
	{0x00000000, 0x00},	// 240
	{0x00000000, 0x00},	// 241
	{0x00000000, 0x00},	// 242
	{0x00000000, 0x00},	// 243
	{0x00000000, 0x00},	// 244
	{0x00000000, 0x00},	// 245
	{0x00000000, 0x00},	// 246
	{0x00000000, 0x00},	// 247
	{0x00000000, 0x00},	// 248
	{0x00000000, 0x00},	// 249
	{0x00000000, 0x00},	// 250
	{0x00000000, 0x00},	// 251
	{0x00000000, 0x00},	// 252
	{0x00000000, 0x00},	// 253
	{0x00000000, 0x00},	// 254
	{0x00000000, 0x00},	// 255
};
