#pragma once

#include <string>
#include <algorithm>
#include <map>

namespace Algebra
{
	template<class Type>
	class Polynomial
	{
	private:
		std::map<int, Type> coef;

	private:
		constexpr Polynomial()
		{
		}

		void cleanup()
		{
			for(auto i = coef.begin(); i != coef.end(); )
				if(i->second == 0)
					coef.erase(i++);
				else
					++i;
		}

	public:
		Polynomial(const Polynomial & that)
			: coef(that.coef)
		{
		}

		const Polynomial operator+(const Polynomial & that) const
		{
			Polynomial result;

			for(auto i = coef.begin(); i != coef.end(); ++i)
				result.coef[i->first] += i->second;

			for(auto i = that.coef.begin(); i != that.coef.end(); ++i)
				result.coef[i->first] += i->second;

			result.cleanup();
			return result;
		}

		const Polynomial operator-(const Polynomial & that) const
		{
			Polynomial result;

			for(auto i = coef.begin(); i != coef.end(); ++i)
				result.coef[i->first] += i->second;

			for(auto i = that.coef.begin(); i != that.coef.end(); ++i)
				result.coef[i->first] -= i->second;

			result.cleanup();
			return result;
		}

		const Polynomial operator-() const
		{
			Polynomial result;

			for(auto i = coef.begin(); i != coef.end(); ++i)
				result.coef[i->first] -= i->second;

			result.cleanup();
			return result;
		}

		const Polynomial operator*(const Polynomial & that) const
		{
			Polynomial result;

			for(auto i = coef.begin(); i != coef.end(); ++i)
				for(auto j = that.coef.begin(); j != that.coef.end(); ++j)
					result.coef[i->first + j->first] += i->second * j->second;

			result.cleanup();
			return result;
		}

		const Polynomial power(size_t p) const
		{
			Polynomial result = constant(1);
			Polynomial m = *this;

			for( ; p > 0; p >>= 1)
			{
				if(p & 1)
					result = result * m;
				m = m * m;
			}

			return result;
		}

		bool operator==(const Polynomial & that) const
		{
			return coef == that.coef;
		}

		bool operator!=(const Polynomial & that) const
		{
			return !operator==(that);
		}

		std::string toString() const
		{
			if(coef.empty())
				return "0";

			std::ostringstream str;
			for(auto i = coef.begin(); i != coef.end(); ++i)
			{
				if(i != coef.begin())
					str << " + ";

				str << i->second;

				const auto p = i->first;
				if(p != 0)
				{
					str << "x";
					if(p != 1)
					{
						str << "^";
						if(p > 0)
							str << p;
						else
							str << "(" << p << ")";
					}
				}
			}

			return str.str();
		}

	public:
		static const Polynomial constant(const Type & c)
		{
			Polynomial v;
			v.coef[0] = c;
			v.cleanup();
			return v;
		}

		static const Polynomial x(const Type & c)
		{
			Polynomial x;
			x.coef[1] = c;
			x.cleanup();
			return x;
		}

		static const Polynomial invx(const Type & c)
		{
			Polynomial invx;
			invx.coef[-1] = c;
			invx.cleanup();
			return invx;
		}
	};
}
