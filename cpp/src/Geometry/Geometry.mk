
Geometry_CPP := $(wildcard Geometry/*.cpp)
srcList := $(srcList) $(Geometry_CPP)

.PHONY: Geometry
Geometry: Util Geometry/Geometry.mk $(call oFileFromSrc, $(Geometry_CPP))
