#include <fstream>
#include "PSPrinter.h"

using namespace Graph;
using Geometry::Vector2D;

namespace Graph
{
	class PagePrinter
	{
	public:
		PagePrinter(bool = true);
		~PagePrinter();
		std::string getCode() const;
		void print(const std::string &);
		void exact(const std::string &);

	private:
		void putWS();
		std::string processNumber(const std::string &) const;

	private:
		std::ostringstream out;
		bool is_whitespace;
		bool is_identifier;
		bool is_number;
		unsigned line_offset;
		const unsigned line_width;
		const bool compress_id;
	};
}

static const char * shortcuts[] [2] = {
	{"arc"         , "a" },
	{"closepath"   , "c" },
	{"curveto"     , "k" },
	{"fill"        , "f" },
	{"grestore"    , "gr"},
	{"gsave"       , "gs"},
	{"lineto"      , "l" },
	{"moveto"      , "m" },
	{"newpath"     , "n" },
	{"scale"       , "sc"},
	{"setdash"     , "sd"},
	{"setlinewidth", "sw"},
	{"setrgbcolor" , "sg"},
	{"stroke"      , "s" },
	{"translate"   , "t" },
	{"showpage"    , "sp"},
};

PagePrinter::PagePrinter(bool cid)
	: is_whitespace(true)
	, is_identifier(false)
	, is_number(false)
	, line_offset(0)
	, line_width(128)
	, compress_id(cid)
{
}

PagePrinter::~PagePrinter()
{
}

std::string PagePrinter::getCode() const
{
	return out.str();
}

void PagePrinter::print(const std::string & code)
{
	std::string buffer;
	for(unsigned pos = 0; pos < code.length(); pos++)
	{
		char cur = code[pos];

		if(!is_number && ((cur >= 'a' && cur <= 'z') || (cur >= 'A' && cur <= 'Z') || cur == '_'))
		{
			if(!is_identifier)
				buffer.clear();
			buffer += cur;
			is_identifier = true;
			is_whitespace = false;
			continue;
		}

		if(is_identifier && cur >= '0' && cur <= '9')
		{
			buffer += cur;
			continue;
		}

		if(is_identifier)
		{
			is_identifier = false;

			if(compress_id)
			{
				for(unsigned i = 0; i < sizeof(shortcuts) / (2 * sizeof(const char *)); i++)
					if(buffer == shortcuts[i] [0])
					{
						buffer = shortcuts[i] [1];
						break;
					}
			}

			out << buffer;
			line_offset += buffer.length();
		}

		if((cur >= '0' && cur <= '9') || cur == '-' || cur == '+' || cur == '.' || cur == 'e' || cur == 'E')
		{
			if(!is_number)
				buffer.clear();
			buffer += cur;
			is_number = true;
			is_whitespace = false;
			continue;
		}

		if(is_number)
		{
			buffer = processNumber(buffer);
			out << buffer;
			line_offset += buffer.length();
			is_number = false;
		}

		if(cur == ' ' || cur == '\n' || cur == '\t' || cur == 0)
		{
			if(!is_whitespace)
				putWS();
			is_whitespace = true;
			continue;
		}
		else is_whitespace = false;

		out << cur;
		line_offset++;
	}
}

void PagePrinter::exact(const std::string & s)
{
	out << s;
	line_offset += s.length();
	is_whitespace = false;
}

void PagePrinter::putWS()
{
	line_offset++;
	if(line_offset > line_width)
	{
		out << "\n";
		line_offset = 0;
	}
	else out << " ";
}

std::string PagePrinter::processNumber(const std::string & s) const
{
	std::string res;
	bool sign = false, point = false;
	unsigned state = 0, zeroes = 0;
	for(unsigned i = 0; i < s.length(); i++)
	{
		if(state == 0)
		{
			if(s[i] == '-')
			{
				sign = !sign;
				continue;
			}
			else if(s[i] == '+')
				continue;
			else state++;
		}

		if(state == 1)
		{
			if(s[i] == '0')
				continue;
			else state++;
		}

		if(state == 2)
		{
			if(s[i] >= '0' && s[i] <= '9')
			{
				res += s[i];
				continue;
			}
			else if(s[i] == '.')
			{
				state++;
				continue;
			}
			else if(s[i] == 'e' || s[i] == 'E')
			{
				res += 'e';
				state = 4;
				continue;
			}
			else return "";
		}

		if(state == 3)
		{
			if(s[i] >= '1' && s[i] <= '9')
			{
				if(!point)
				{
					res += '.';
					point = true;
				}

				while(zeroes > 0)
				{
					res += '0';
					zeroes--;
				}

				res += s[i];
				continue;
			}
			else if(s[i] == '0')
			{
				zeroes++;
				continue;
			}
			else if(s[i] == 'e' || s[i] == 'E')
			{
				res += 'e';
				state = 4;
				continue;
			}
			else return "";
		}

		if(res.size() == 0)
			return "0";

		if(state == 4)
		{
			res += s[i];
		}
	}

	if(res.size() == 0)
		return "0";

	if(sign)
		return "-" + res;
	return res;
}


PSPrinter::PSPrinter(const std::string & f)
	: written(false)
	, filename(f)
	, prolog(new PagePrinter(false))
	, width(612.0)
	, height(792.0)
	, horizontal_pad(20.0)
	, vertical_pad(30.0)
	, unit_size(100)
	, text_size(10)
	, text_interval(1.5)
{
	prolog->print("/Times-Roman findfont setfont\n");
	for(unsigned i = 0; i < sizeof(shortcuts) / (2 * sizeof(const char *)); i++)
		prolog->print(std::string("/") + shortcuts[i] [1] + " { " + shortcuts[i] [0] + " } def\n");

	newPage();
}

PSPrinter::~PSPrinter()
{
	if(!written)
		writeFile();

	delete prolog;
	for(unsigned i = 0; i < pages.size(); i++)
		delete pages[i];
}

void PSPrinter::writeFile()
{
	if(written)
		return;
	written = true;

	std::ofstream out(filename.c_str());

	out << "%!PS-Adobe-2.0\n";
	out << "%%BoundingBox: 0 0 " << width << " " << height << "\n";
	out << "%%Pages: " << pages.size() << "\n";
	out << "%%PageOrder: Ascend\n";
	out << "%%EndComments\n";
	out << prolog->getCode() << "\n";
	out << "%%EndProlog\n";

	for(unsigned page = 0; page < pages.size(); page++)
	{
		out << "%%Page: page" << page + 1 << " " << page + 1 << "\n";
		PagePrinter & printer = *pages[page];
		printer.print("restore showpage ");
		out << printer.getCode() << "\n";
	}

	out << "%%Trailer\n";
	out << "%%EOF\n";
}

void PSPrinter::newPage()
{
	pages.push_back(new PagePrinter());
	cur_page = pages.size() - 1;
	pages[cur_page]->print("save\n");

	x = horizontal_pad;
	y = height - vertical_pad;
}

void PSPrinter::newLine(bool separator)
{
	if(x > horizontal_pad)
	{
		x = horizontal_pad;
		y -= unit_size;
		if(y - unit_size <= vertical_pad)
			newPage();
	}

	if(separator)
	{
		std::ostringstream pos;
		pos << horizontal_pad << " " << y << " moveto " << width - horizontal_pad << " " << y << " lineto stroke\n";
		PagePrinter & printer = *pages[cur_page];
		printer.print(pos.str());
	}
}

void PSPrinter::print(const std::string & code)
{
	std::ostringstream pos;
	pos << x + 0.5 * unit_size << " " << y - 0.5 * unit_size << " translate\n";
	pos << 0.40 * unit_size << " " << 0.40 * unit_size << " scale\n";

	PagePrinter & printer = *pages[cur_page];
	printer.print("gsave\n");
	printer.print(pos.str());
	printer.print(code);
	printer.print(" grestore\n");

	x += unit_size;
	if(x + unit_size >= width - horizontal_pad)
		newLine();
}

void PSPrinter::text(const std::string & text, double size)
{
	if(size == 0.0)
		size = text_size;

	std::ostringstream pos, nl;
	pos << x << " " << y << " translate\n";
	pos << size << " " << size << " scale\n";
	nl << "0 " << -text_interval << " translate\n";

	PagePrinter & printer = *pages[cur_page];
	printer.print("gsave\n");
	printer.print(pos.str());

	unsigned lines = 0;
	std::string buffer;
	for(unsigned i = 0; i < text.length(); i++)
	{
		char cur = text[i];
		if(cur == '\n')
		{
			if(lines > 0)
				printer.print(nl.str());
			if(buffer != "")
			{
				printer.print("0 -1 moveto (");
				printer.exact(buffer);
				printer.print(") show\n");
				buffer = "";
			}
			lines++;
		}
		else buffer += cur;
	}

	if(buffer != "")
	{
		printer.print("0 -1 moveto (");
		printer.exact(buffer);
		printer.print(") show\n");
	}

	printer.print("grestore\n");

	y -= size * text_interval * lines;
}	
