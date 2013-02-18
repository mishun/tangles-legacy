
Topology_CPP = $(wildcard Topology/*.cpp)
srcList := $(srcList) $(Topology_CPP)

.PHONY: Topology
Topology: Util Topology/Topology.mk $(call oFileFromSrc, $(Topology_CPP))
