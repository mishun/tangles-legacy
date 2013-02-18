#pragma once

#include <vector>
#include <string>
#include <sstream>
#include <Geometry/Vector2D.h>
#include "Graph.h"
#include "Path.h"

namespace Graph
{
	class PagePrinter;

	class PSPrinter
	{
	public:
		PSPrinter(const std::string &);
		~PSPrinter();
		void writeFile();
		void newPage();
		void newLine(bool = false);
		void print(const std::string &);
		void text(const std::string &, double = 0);

	private:
		bool written;
		const std::string filename;
		PagePrinter * prolog;
		std::vector<PagePrinter *> pages;
		unsigned cur_page;

		const double width;
		const double height;
		double horizontal_pad;
		double vertical_pad;
		double x;
		double y;
		double unit_size;
		double text_size;
		double text_interval;

	public:
		class Image
		{
		public:
			Image();

			void stroke(const Path &);
			void fill(const Path &);
			void fill(const Path &, const Color &);

			void setScale(double);
			void setLineWidth(double);
			void setDash(double, double);
			std::string toString() const;

		private:
			std::ostringstream str;
		};
	};
}
