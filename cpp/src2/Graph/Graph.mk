
Graph_CPP := $(wildcard Graph/*.cpp)
srcList := $(srcList) $(Graph_CPP)

.PHONY: Graph
Graph: Util Geometry Graph/Graph.mk $(call oFileFromSrc, $(Graph_CPP))
