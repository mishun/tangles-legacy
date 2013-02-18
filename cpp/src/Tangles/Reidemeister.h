#pragma once

#include <cassert>
#include <utility>
#include <algorithm>
#include <vector>
#include <queue>
#include <iostream>
#include "./TangleUtil.h"
#include "./TangleStorage.h"
#include "./Crossings/ArbitraryCrossing.h"

namespace Tangles
{
	class Reidemeister
	{
	private:
		static bool reduce1st(std::vector< std::vector< std::pair<size_t, size_t> > > & g, std::vector<bool> & active, bool & bad)
		{
			for(size_t a = 1; a < g.size(); a++)
			{
				if(!active[a])
					continue;

				for(size_t i = 0; i < 4; i++)
				{
					const size_t b = g[a] [i].second;
					const size_t j = g[a] [i].first;

					if(b == a && j == ((i + 1) & 3))
					{
						active[a] = false;

						const auto p = g[a] [ (i + 2) & 3 ];
						const auto q = g[a] [ (i + 3) & 3 ];

						if(a == p.second || a == q.second)
						{
							bad = true;
							return false;
						}

						g[p.second] [p.first] = q;
						g[q.second] [q.first] = p;

						return true;
					}
				}
			}

			return false;
		}

		template<class Tangle>
		static bool reduce2nd(const Tangle & tangle, std::vector< std::vector< std::pair<size_t, size_t> > > & g, std::vector<bool> & active, bool & bad)
		{
			for(size_t a = 1; a < g.size(); a++)
			{
				if(!active[a])
					continue;

				for(size_t i = 0; i < 4; i++)
				{
					const size_t b = g[a] [i].second;
					const size_t j = g[a] [i].first;

					if(b != 0 && b != a && b == g[a] [ (i + 1) & 3 ].second)
					{
						//assert(b != a);
						//assert(g[b] [(j + 3) & 3].second == a);
						//assert(b != 0);
						//assert(active[b]);

						if(ArbitraryCrossing::passOver(tangle, Dart(i, a)) == ArbitraryCrossing::passOver(tangle, Dart(j, b)))
						{
							active[a] = false;
							active[b] = false;

							const auto p = g[a] [(i + 2) & 3];
							const auto q = g[a] [(i + 3) & 3];
							const auto r = g[b] [(j + 1) & 3];
							const auto s = g[b] [(j + 2) & 3];

							if(p.second == a || p.second == b || q.second == a || q.second == b ||
								r.second == a || r.second == b || s.second == a || s.second == b)
							{
								bad = true;
								return false;
							}

							g[p.second] [p.first] = s;
							g[q.second] [q.first] = r;
							g[r.second] [r.first] = q;
							g[s.second] [s.first] = p;

							return true;
						}
					}
				}
			}

			return false;
		}

		template<class Tangle>
		static bool untangleReidemeister(const Tangle & tangle, std::vector< std::vector< std::pair<size_t, size_t> > > & g, std::vector<bool> & active, bool & bad)
		{
			bool action = false;
			while(true)
			{
				if(reduce1st(g, active, bad))
				{
					action = true;
					continue;
				}

				if(bad)
					return false;

				if(reduce2nd(tangle, g, active, bad))
				{
					action = true;
					continue;
				}

				if(bad)
					return false;

				break;
			}

			return action;
		}

		static bool untangleWeak(std::vector< std::vector< std::pair<size_t, size_t> > > & g, std::vector<bool> & active, bool & bad)
		{
			auto & b = g[0];

			for(size_t i = 0; i < b.size(); i++)
			{
				const auto legA = b[i];
				const auto legB = b[(i + 1) % b.size()];

				if(legA.second == 0 || legB.second == 0)
				{
					bad = true;
					return false;
				}

				if(legA.second == legB.second && legB.first == ((legA.first + 1) & 3))
				{
					const size_t a = legA.second;
					const size_t j = legA.first;

					const auto p = g[a] [j];
					const auto q = g[a] [ (j + 1) & 3 ];
					const auto r = g[a] [ (j + 2) & 3 ];
					const auto s = g[a] [ (j + 3) & 3 ];

					assert(p.second == 0 && p.first == i);
					assert(q.second == 0);

					if(r.second == 0 && s.second == 0)
					{
						bad = true;
						return false;
					}

					active[a] = false;
					g[p.second] [p.first] = s;
					g[q.second] [q.first] = r;
					g[r.second] [r.first] = q;
					g[s.second] [s.first] = p;

					return true;
				}
			}

			return false;
		}

		template<class Tangle>
		static std::vector< std::vector< std::pair<size_t, size_t> > > unassemble(const Tangle & tangle)
		{
			std::vector< std::vector< std::pair<size_t, size_t> > > g(tangle.numberOfCrossings() + 1);
			auto & b = g[0];

			{
				for(size_t i = 1; i <= tangle.numberOfCrossings(); i++)
				{
					auto & v = g[i];
					v.resize(4);

					for(size_t j = 0; j < 4; j++)
					{
						const Dart d = tangle.neighbour(Dart(j, i));
						v[j] = std::make_pair((size_t)d.place, (size_t)d.crossing);
					}
				}

				{
					b.resize(tangle.numberOfLegs());

					auto l = tangle.baseLeg();
					for(size_t i = 0; i < tangle.numberOfLegs(); i++, l = l.nextCCW())
					{
						const Dart d = l.leg();
						b[i] = std::make_pair((size_t)d.place, (size_t)d.crossing);
						g[d.crossing] [d.place] = std::make_pair(i, static_cast<size_t>(0));
					}
				}
			}

			return g;
		}

		template<template<typename C> class Tangle, typename C, class Graph>
		static Data::SharedPointer< TangleStorage<C> > assemble(const Tangle<C> & tangle, const Graph & g, const std::vector<bool> & active)
		{
			auto & b = g[0];

			std::vector<C> c(1);
			std::vector<Algebra::D4Group> r(1);
			std::vector< std::vector<Dart> > n;
			n.push_back(std::vector<Dart>(b.size()));

			std::vector<size_t> live;
			std::vector<size_t> liveIndex(g.size());

			for(size_t i = 1; i < g.size(); i++)
				if(active[i])
				{
					live.push_back(i);
					liveIndex[i] = n.size();

					n.push_back(std::vector<Dart>(4));
					c.push_back(tangle.crossing(i));
					r.push_back(tangle.rotation(i));
				}
				else
					liveIndex[i] = 0;

			for(size_t i = 0; i < live.size(); i++)
				for(size_t j = 0; j < 4; j++)
					n[i + 1] [j] = Dart(g[ live[i] ] [j].first, liveIndex[ g[ live[i] ] [j].second ]);

			for(size_t i = 0; i < b.size(); i++)
				n[0] [i] = Dart(b[i].first, liveIndex[ b[i].second ]);


			for(size_t i = 0; i < b.size(); i++)
				if(b[i].second == 0)
					return Data::SharedPointer< TangleStorage<C> >();

			auto res = Data::SharedPointer< TangleStorage<C> >(new TangleStorage<C>(live.size(), b.size(), n, c, r, n[0]));
			if(!connected(*res))
				return Data::SharedPointer< TangleStorage<C> >();

			return res;
		}

	public:
		template<template<typename C> class Tangle, typename C>
		static std::vector< Data::SharedPointer< TangleStorage<C> > > neighbours3rdReidemeister(const Tangle<C> & tangle)
		{
			std::vector< Data::SharedPointer< TangleStorage<C> > > result;

			for(size_t i = 1; i <= tangle.numberOfCrossings(); i++)
				for(size_t j = 0; j < 4; j++)
				{
					const Dart a(j, i);
					const Dart b = tangle.neighbour(a);
					const Dart c = tangle.neighbour(a.nextCCW());

					if(b.onBorder() || c.onBorder())
						continue;

					if(a.crossing == b.crossing || a.crossing == c.crossing || b.crossing == c.crossing)
						continue;

					if(tangle.neighbour(c.nextCCW()) != b.nextCW())
						continue;

					if(ArbitraryCrossing::passOver(tangle, b) != ArbitraryCrossing::passOver(tangle, c))
						continue;

					auto g = unassemble(tangle);

					assert(a.crossing != 0 && b.crossing != 0 && c.crossing != 0);

					const auto p = g[a.crossing] [ (a.place + 2) & 3 ];
					const auto q = g[a.crossing] [ (a.place + 3) & 3 ];
					const auto r = g[b.crossing] [ (b.place + 2) & 3 ];
					const auto s = g[c.crossing] [ (c.place + 2) & 3 ];

					if(p.second == a.crossing || p.second == b.crossing || p.second == c.crossing)
						continue;

					if(q.second == a.crossing || q.second == b.crossing || q.second == c.crossing)
						continue;

					if(r.second == a.crossing || r.second == b.crossing || r.second == c.crossing)
						continue;

					if(s.second == a.crossing || s.second == b.crossing || s.second == c.crossing)
						continue;

					g[a.crossing] [a.place] = r;
					g[a.crossing] [ (a.place + 1) & 3 ] = s;
					g[a.crossing] [ (a.place + 2) & 3 ] = c.opposite().toPair();
					g[a.crossing] [ (a.place + 3) & 3 ] = b.opposite().toPair();

					g[b.crossing] [b.place] = q;
					g[b.crossing] [ (b.place + 2) & 3 ] = a.nextCW().toPair();

					g[c.crossing] [c.place] = p;
					g[c.crossing] [ (c.place + 2) & 3 ] = a.opposite().toPair();

					g[p.second] [p.first] = c.toPair();
					g[q.second] [q.first] = b.toPair();
					g[r.second] [r.first] = a.toPair();
					g[s.second] [s.first] = a.nextCCW().toPair();


					std::vector<bool> active(tangle.numberOfCrossings() + 1, true);
					bool bad = false;
					untangleReidemeister(tangle, g, active, bad);

					if(!bad)
					{
						auto current = assemble(tangle, g, active);
						if(!current.null())
							result.push_back(current);
					}
					else
						result.push_back(Data::SharedPointer< TangleStorage<C> >());
				}

			return result;
		}

		template<template<typename C> class Tangle, typename C>
		static Data::SharedPointer< TangleStorage<C> > greedyReidemeisterReduction(const Tangle<C> & tangle)
		{
			auto g = unassemble(tangle);

			std::vector<bool> active(tangle.numberOfCrossings() + 1, true);
			bool bad = false;
			untangleReidemeister(tangle, g, active, bad);

			if(bad)
				return Data::SharedPointer< TangleStorage<C> >();
			else
				return assemble(tangle, g, active);
		}

		template<template<typename C> class Tangle, typename C>
		static Data::SharedPointer< TangleStorage<C> > greedyWeakReduction(const Tangle<C> & tangle)
		{
			auto g = unassemble(tangle);

			std::vector<bool> active(tangle.numberOfCrossings() + 1, true);
			bool bad = false;
			while(true)
			{
				if(untangleReidemeister(tangle, g, active, bad))
					continue;

				if(bad)
					break;

				if(untangleWeak(g, active, bad))
					continue;

				break;
			}

			if(bad)
				return Data::SharedPointer< TangleStorage<C> >();
			else
				return assemble(tangle, g, active);
		}
	};
}
