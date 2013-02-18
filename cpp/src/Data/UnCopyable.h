#pragma once

namespace Data
{
	class UnCopyable
	{
	public:
		UnCopyable() {}

	private:
		UnCopyable(const UnCopyable &);
		const UnCopyable & operator=(const UnCopyable &);
	};

	class UnCopyableEquatable : public UnCopyable
	{
	public:
		bool operator==(const UnCopyableEquatable & that) const
		{
			return this == &that;
		}

		bool operator!=(const UnCopyableEquatable & that) const
		{
			return this != &that;
		}
	};
}
