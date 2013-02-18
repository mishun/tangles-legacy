#pragma once

namespace Util
{
	class IDestroyable
	{
	protected:
		IDestroyable() {}
		virtual ~IDestroyable() {}

	public:
		virtual void destroy() = 0;
	};
}
