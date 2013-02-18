#pragma once

#include <cassert>
#include <cstring>
#include <vector>
#include <Algebra/D4Group.h>
#include "./IncrementalTangle.h"

namespace Tangles
{
	template<typename C>
	class IncrementalFastTangle : public IncrementalTangle<C>
	{
	private:
		using IncrementalTangle<C>::maxStackDepth;
		using IncrementalTangle<C>::stackDepth;

	public:
		using typename IncrementalTangle<C>::LegIterator;
		using IncrementalTangle<C>::numberOfCrossings;
		using IncrementalTangle<C>::numberOfLegs;
		using IncrementalTangle<C>::neighbour;
		using IncrementalTangle<C>::crossing;
		using IncrementalTangle<C>::rotation;
		using IncrementalTangle<C>::baseLeg;

	private:
		struct StateDump
		{
			size_t cutpointsMask;
			Algebra::DnSubGroup symmetry;
		};

		struct DfsResult
		{
			size_t fup;
			size_t border;
		};

	private:
		StateDump dumpStack[maxStackDepth];

	public:
		IncrementalFastTangle(const C & crossing)
			: IncrementalTangle<C>(crossing)
		{
		}

		virtual ~IncrementalFastTangle()
		{
		}

		virtual const Algebra::DnSubGroup & symmetry() const //override
		{
			return dumpStack[stackDepth].symmetry;
		}

		size_t getCutpoints() const
		{
			return dumpStack[stackDepth].cutpointsMask;
		}

	protected:
		virtual bool preCheck(typename IncrementalTangle<C>::LegIterator const &, size_t legsToGlue, const Algebra::D4Group &, const C &) //override
		{
			if(numberOfLegs() + 4 - 2 * legsToGlue < 4)
				return false;

			return true;
		}

		virtual bool postCheck(size_t lastCrossing, size_t legsToGlue) //override
		{
			using Algebra::DnSubGroup;

			size_t cutpointsMask = 0;
			if(legsToGlue == 1)
			{
				if(numberOfCrossings() > 2)
					cutpointsMask = dumpStack[stackDepth - 1].cutpointsMask | (1 << neighbour(Dart(0, lastCrossing)).crossing);
			}
			else
			{
				size_t timer = 1, cutpoints = 0;
				std::vector<size_t> tin(numberOfCrossings() + 1, 0);

				const DfsResult result = dfs(lastCrossing, 0, timer, cutpoints, &tin.front());
				if(legsToGlue == 3 && result.border == 1)
					return false;

				cutpointsMask = cutpoints & ~(1 << lastCrossing);
			}


			const size_t fastRootCode = fastCode(baseLeg().leg().crossing);
			{
				auto l = baseLeg();
				for(size_t i = 0; i < numberOfLegs(); i++, l = l.nextCCW())
				{
					const Dart cur = l.leg();
					if(cur.crossing != l.nextCW().leg().crossing && (cutpointsMask & (1 << cur.crossing)) == 0)
					{
						const size_t fc = fastCode(l.leg().crossing);
						if(fc < fastRootCode)
							return false;
					}
				}
			}

			int rootDir;
			size_t symmetryDir = 0;
			size_t symmetryRev = 0;
			size_t positionDir = 0;
			size_t positionRev = 3 - legsToGlue;

			std::vector<size_t> rcode(2 * numberOfCrossings(), 0xFFFFFFFF);

			{
				//const size_t dirMask = fastDirectionMask(lastCrossing());

				rootDir = 1;
				symmetryDir = 1;
				positionDir = 0;

				Dart start = Dart(legsToGlue, lastCrossing);
				//if(dirMask & 2)
					tryRootCode(&rcode.front(), start, 1);

				start.place = 3;
				//if(dirMask & 1)
				{
					int compareResult = tryRootCode(&rcode.front(), start, -1);

					if(compareResult > 0)
					{
						rootDir = -1;
						symmetryDir = 0;
					}

					if(compareResult >= 0)
					{
						symmetryRev++;
					}
				}
			}

			{
				auto l = baseLeg();
				for(size_t i = 0; i < numberOfLegs(); i++, l = l.nextCCW())
				{
					const Dart cur = l.leg();

					if(cur.crossing == lastCrossing || cutpointsMask & (1 << cur.crossing))
						continue;

					if(fastCode(cur.crossing) > fastRootCode)
						continue;

					if(cur.crossing != l.nextCCW().leg().crossing)
					{
						int compareResult = compareRootCode(&rcode.front(), cur, -1);

						if(compareResult > 0)
							return false;

						if(compareResult == 0)
						{
							symmetryRev++;
							positionRev = i;
						}
					}

					if(cur.crossing != l.nextCW().leg().crossing)
					{
						int compareResult = compareRootCode(&rcode.front(), cur, 1);

						if(compareResult > 0)
							return false;

						if(compareResult == 0)
						{
							symmetryDir++;
							positionDir = i;
						}
					}
				}
			}

			assert(symmetryDir == 0 || symmetryRev == 0 || symmetryDir == symmetryRev);

			{
				StateDump & stateDump = dumpStack[stackDepth];
				stateDump.cutpointsMask = cutpointsMask;

				const size_t period = numberOfLegs() / std::max(symmetryDir, symmetryRev);
				if(symmetryDir == symmetryRev)
					stateDump.symmetry = DnSubGroup::createFromPeriodAndMirroredZero(numberOfLegs(), period, positionRev + positionDir);
				else
					stateDump.symmetry = DnSubGroup::createFromPeriod(numberOfLegs(), period);
			}

			return true;
		}

	private:
		DfsResult dfs(const size_t v, const size_t from, size_t & timer, size_t & cutpoint, size_t tin[]) const
		{
			DfsResult result;
			result.fup = tin[v] = timer++;
			result.border = 0;

			for(size_t i = 0; i < 4; i++)
			{
				const size_t to = neighbour(Dart(i, v)).crossing;

				if(to == from)
					continue;

				if(to == 0)
				{
					result.border++;
					continue;
				}

				if(tin[to] > 0)
				{
					if(tin[to] < result.fup)
						result.fup = tin[to];
				}
				else
				{
					const DfsResult that = dfs(to, v, timer, cutpoint, tin);

					if(that.fup < result.fup)
						result.fup = that.fup;

					if(that.fup >= tin[v])
						cutpoint |= (1 << v);

					if(that.fup <= tin[v])
						result.border += that.border;
					else
						result.border++;
				}
			}

			return result;
		}

		size_t fastCode(const size_t start) const
		{
			size_t id = 0, free_id = 1, code = 0;
			for(size_t i = 0; i < 4; i++)
			{
				code <<= 2;

				size_t cur = neighbour(Dart(i, start)).crossing;

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

			assert(free_id <= 4);

			return vertex_table[code] [0];
		}

		size_t fastDirectionMask(const size_t start) const
		{
			size_t id = 0, free_id = 1, code = 0;
			for(size_t i = 0; i < 4; i++)
			{
				code <<= 2;

				size_t cur = neighbour(Dart(i, start)).crossing;

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

			assert(free_id <= 4);

			size_t result = 0;

			const size_t mask = vertex_table[code] [1];

			if(mask & 0x55)
				result |= 1;

			if(mask & 0xAA)
				result |= 2;

			return result;
		}

		int tryRootCode(size_t rcode[], const Dart start, const int dir) const
		{
			using Algebra::D4Group;

			std::vector<size_t> id(numberOfCrossings() + 1, 0);
			id[start.crossing] = 1;

			size_t tail = 0;
			std::vector<Dart> queue(numberOfCrossings());
			queue[tail++] = start;

			size_t free_id = 2;
			bool better = false;
			for(size_t head = 0; head < numberOfCrossings(); head++)
			{
				const Dart base = queue[head];
				size_t vcode = 0;

				Dart it = base;
				for(size_t i = 0; i < 4; i++, it = it.next(dir))
				{
					const Dart neigh = neighbour(it);
					vcode <<= 6;

					if(!neigh.onBorder())
					{
						if(id[neigh.crossing] == 0)
						{
							id[neigh.crossing] = free_id++;
							queue[tail++] = neigh;
						}
						vcode |= id[neigh.crossing];
					}
				}

				if(better || vcode < rcode[head])
				{
					rcode[head] = vcode;
					better = true;
				}
				else if(vcode > rcode[head])
					return -1;
			}

			if(better)
				return 1;
			return 0;
		}

		int compareRootCode(const size_t rcode[], const Dart start, const int dir) const
		{
			using Algebra::D4Group;

			std::vector<size_t> id(numberOfCrossings() + 1, 0);
			id[start.crossing] = 1;

			size_t tail = 0;
			std::vector<Dart> queue(numberOfCrossings());
			queue[tail++] = start;

			size_t free_id = 2;
			for(size_t head = 0; head < numberOfCrossings(); head++)
			{
				const Dart base = queue[head];
				size_t vcode = 0;

				Dart it = base;
				for(size_t i = 0; i < 4; i++, it = it.next(dir))
				{
					const Dart neigh = neighbour(it);
					vcode <<= 6;

					if(!neigh.onBorder())
					{
						if(id[neigh.crossing] == 0)
						{
							id[neigh.crossing] = free_id++;
							queue[tail++] = neigh;
						}
						vcode |= id[neigh.crossing];
					}
				}

				if(vcode < rcode[head])
					return 1;

				if(vcode > rcode[head])
					return -1;
			}

			return 0;
		}

	private:
		static const size_t vertex_table[256] [2];
	};

	template<class C>
	const size_t IncrementalFastTangle<C>::vertex_table[256] [2] = {
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
}
