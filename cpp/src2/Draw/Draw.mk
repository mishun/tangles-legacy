
Draw_CPP := $(wildcard Draw/*.cpp)
srcList := $(srcList) $(Draw_CPP)

.PHONY: Draw
Draw: Util Geometry Topology Graph Draw/Draw.mk $(call oFileFromSrc, $(Draw_CPP))
