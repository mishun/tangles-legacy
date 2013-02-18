#pragma once

#include "IncrementalTangleTemplate.h"
#include "SubTangle.h"

namespace Tangles
{
	class IncrementalAlternatingTangle : private IncrementalTangleTemplate
	{
	private:
		size_t crossings;

	public:
		IncrementalAlternatingTangle();
		virtual ~IncrementalAlternatingTangle();

		bool insertSubTangle(const SubTangle & sub, D4Group orientation);

	public:
		using IncrementalTangleTemplate::numberOfLegs;

		size_t numberOfTemplateVertices() const
		{
			return IncrementalTangleTemplate::numberOfCrossings();
		}

		size_t numberOfTemplateEdges() const
		{
			return IncrementalTangleTemplate::numberOfEdges();
		}

		size_t numberOfCrossings() const
		{
			return crossings;
		}
	};
}
