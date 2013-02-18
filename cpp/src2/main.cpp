#include <ctime>
#include <iostream>
#include <vector>
#include <utility>
#include <Geometry/Geometry.h>
#include <Topology/EmbeddedGraph.h>
#include <Graph/PSPrinter.h>
#include <Draw/CircleDrawer.h>
#include <Draw/GluedSurfaceDrawer.h>

using namespace Topology;

Graph::PSPrinter ps("output.ps");

void printInfo(EmbeddedGraph & eg)
{
	std::cout << "v = " << eg.numberOfVertexes() << "; f = " << eg.numberOfFaces() << "; e = " << eg.numberOfEdges() << "\n";
	std::cout << "g = " << eg.genus() << "\n";
	std::cout << "isTriangulation = " << eg.isTriangulation() << "\n";
//	std::cout << eg.toString() << "\n";
}

void test1()
{
	std::vector< std::pair<size_t, size_t> > v;

	v.push_back(std::make_pair(0, 1));
	v.push_back(std::make_pair(0, 0));
	v.push_back(std::make_pair(0, 3));
	v.push_back(std::make_pair(0, 2));

	std::vector< std::vector< std::pair<size_t, size_t> > > g;
	g.push_back(v);

	EmbeddedGraph & eg = EmbeddedGraph::createFromVertexAdjacencyList(g);
	EmbeddedGraph & dg = eg.dualGraph();
	EmbeddedGraph & der = eg.derivableGraph();
	EmbeddedGraph & der2 = der.derivableGraph();
	EmbeddedGraph & der3 = der2.derivableGraph();
	EmbeddedGraph & der4 = der3.derivableGraph();
	EmbeddedGraph & d3 = der3.dualGraph();

	ps.print(Draw::CircleDrawer::draw(eg, 3, false));
	ps.print(Draw::CircleDrawer::draw(eg, 3, true));
	ps.newLine();

	ps.print(Draw::CircleDrawer::draw(dg, 3, false));
	ps.print(Draw::CircleDrawer::draw(dg, 3, true));
	ps.newLine();

	ps.print(Draw::CircleDrawer::draw(der, 2, false));
	ps.print(Draw::CircleDrawer::draw(der, 2, true));
	ps.newLine();

	ps.print(Draw::CircleDrawer::draw(der2, 2, false));
	ps.print(Draw::CircleDrawer::draw(der2, 2, true));
	ps.newLine();

	ps.print(Draw::CircleDrawer::draw(der3, 0, false));
//	ps.print(Draw::CircleDrawer::draw(der3, 0, true));
	ps.print(Draw::CircleDrawer::draw(der3, 1, false));
//	ps.print(Draw::CircleDrawer::draw(der3, 1, true));
	ps.newLine();

//	ps.print(Draw::CircleDrawer::draw(d3, 0, false));
//	ps.print(Draw::CircleDrawer::draw(d3, 0, true));
//	ps.print(Draw::CircleDrawer::draw(d3, 1, false));
//	ps.print(Draw::CircleDrawer::draw(d3, 1, true));
//	ps.newLine();

//	double time = clock();
//	ps.print(Draw::CircleDrawer::draw(der4, 0, false));
//	ps.print(Draw::CircleDrawer::draw(der4, 0, true));
//	time = (clock() - time) / CLOCKS_PER_SEC;
//	std::cerr << "t = " << time << "\n";

	eg.destroy();
	dg.destroy();
	der.destroy();
	der2.destroy();
	der3.destroy();
	der4.destroy();
}

void test2()
{
	std::vector< std::pair<size_t, size_t> > v;

	v.push_back(std::make_pair(0, 3));
	v.push_back(std::make_pair(0, 4));
	v.push_back(std::make_pair(0, 5));
	v.push_back(std::make_pair(0, 0));
	v.push_back(std::make_pair(0, 1));
	v.push_back(std::make_pair(0, 2));

	std::vector< std::vector< std::pair<size_t, size_t> > > g;
	g.push_back(v);

	EmbeddedGraph & eg = EmbeddedGraph::createFromVertexAdjacencyList(g);
	EmbeddedGraph & dg = eg.dualGraph();
	EmbeddedGraph & der = eg.derivableGraph();
	EmbeddedGraph & eder = dg.derivableGraph();
	EmbeddedGraph & der2 = der.derivableGraph();
	EmbeddedGraph & der3 = der2.derivableGraph();
	EmbeddedGraph & der4 = der3.derivableGraph();

	ps.print(Draw::GluedSurfaceDrawer::fastDraw(eg));
	ps.print(Draw::GluedSurfaceDrawer::fastDraw(dg));
	ps.print(Draw::GluedSurfaceDrawer::fastDraw(der));
	ps.print(Draw::GluedSurfaceDrawer::fastDraw(eder));
	//ps.print(Draw::GluedSurfaceDrawer::fastDraw(der2));

	eg.destroy();
	dg.destroy();
	der.destroy();
	eder.destroy();
	der2.destroy();
	der3.destroy();
	der4.destroy();
}

void test3()
{
	std::vector< std::pair<size_t, size_t> > v;

	v.push_back(std::make_pair(0, 2));
	v.push_back(std::make_pair(0, 5));
	v.push_back(std::make_pair(0, 0));
	v.push_back(std::make_pair(0, 6));
	v.push_back(std::make_pair(0, 7));
	v.push_back(std::make_pair(0, 1));
	v.push_back(std::make_pair(0, 3));
	v.push_back(std::make_pair(0, 4));

	std::vector< std::vector< std::pair<size_t, size_t> > > g;
	g.push_back(v);

	EmbeddedGraph & eg = EmbeddedGraph::createFromVertexAdjacencyList(g);
	EmbeddedGraph & dg = eg.dualGraph();
	EmbeddedGraph & der = eg.derivableGraph();
	EmbeddedGraph & eder = dg.derivableGraph();
	EmbeddedGraph & der2 = der.derivableGraph();

	ps.print(Draw::GluedSurfaceDrawer::fastDraw(eg));
	ps.print(Draw::GluedSurfaceDrawer::fastDraw(dg));
	ps.print(Draw::GluedSurfaceDrawer::fastDraw(der));
	ps.print(Draw::GluedSurfaceDrawer::fastDraw(eder));
	//ps.print(Draw::GluedSurfaceDrawer::fastDraw(der2));

	eg.destroy();
	dg.destroy();
	der.destroy();
	eder.destroy();
	der2.destroy();
}

int main()
{
	test1();
	ps.newLine(true);
	test2();
	ps.newLine(true);
	test3();

	return 0;
}
