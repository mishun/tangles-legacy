
Tangles_CPP = $(wildcard Tangles/*.cpp)
srcList := $(srcList) $(Tangles_CPP)

.PHONY: Tangles
Tangles: Util Draw Topology Geometry Graph Tangles/Tangles.mk $(call oFileFromSrc, $(Tangles_CPP))
