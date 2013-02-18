#pragma once

#include <Tangles/IncrementalTangleBase.h>
#include <Tangles/RootCode.h>
#include <ChordDiagrams/ChordGenerator.h>
#include <Topology/EmbeddedGraph.h>

namespace Virtual
{
	class VirtualLink
	{
	public:
		struct Dart
		{
			size_t place : 2;
			size_t vertex : 6;
		};

	private:
		struct Vertex
		{
			Dart neighbours[4];
		};

	private:
		const size_t crossings;
		const size_t _genus;
		Vertex v[32];

	public:
		VirtualLink(const Tangles::IncrementalTangleBase &, const ChordDiagrams::ChordDiagram &, size_t, bool);
		virtual ~VirtualLink();

		Topology::EmbeddedGraph & toGraph() const;
		bool isPrime() const;
		bool isReducable() const;
		Tangles::RootCode rootCode() const;
		bool has4LegPlanarPart() const;

	public:
		size_t numberOfCrossings() const
		{
			return crossings;
		}

		size_t numberOfEdges() const
		{
			return 2 * crossings;
		}

		size_t genus() const
		{
			return _genus;
		}

		Dart neighbour(Dart dart) const
		{
			assert(dart.vertex < crossings);
			return v[dart.vertex].neighbours[dart.place];
		}

		Dart neighbour(size_t vertex, size_t place) const
		{
			assert(vertex < crossings);
			assert(place < 4);
			return v[vertex].neighbours[place];
		}
	};
}
