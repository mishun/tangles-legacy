#pragma once

namespace Data
{
	class IDestroyable
	{
	protected:
		virtual ~IDestroyable() {}

	public:
		virtual void destroy() = 0;
	};
}
