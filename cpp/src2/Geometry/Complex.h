#pragma once

#include <cmath>
#include <string>

namespace Geometry
{
	class Complex
	{
	public:
		double re;
		double im;

	public:
		Complex(double _re = 0.0, double _im = 0.0)
			: re(_re)
			, im(_im)
		{
		}

		double abs() const
		{
			return sqrt(re * re + im * im);
		}

		double abs2() const
		{
			return re * re + im * im;
		}

		Complex operator+() const
		{
			return *this;
		}

		Complex operator-() const
		{
			return Complex(-re, -im);
		}

		Complex operator~() const
		{
			return Complex(re, -im);
		}

		Complex operator+(const Complex & that) const
		{
			return Complex(re + that.re, im + that.im);
		}

		Complex operator-(const Complex & that) const
		{
			return Complex(re - that.re, im - that.im);
		}

		Complex operator*(const Complex & that) const
		{
			return Complex(re * that.re - im * that.im, re * that.im + im * that.re);
		}

		Complex operator/(const Complex & that) const
		{
			double den = that.abs2();
			return Complex((re * that.re + im * that.im) / den, (im * that.re - re * that.im) / den);
		}

		Complex & operator+=(const Complex & that)
		{
			re += that.re;
			im += that.im;
			return *this;
		}

		Complex & operator-=(const Complex & that)
		{
			re -= that.re;
			im -= that.im;
			return *this;
		}

		std::string toString() const;
	};
}
