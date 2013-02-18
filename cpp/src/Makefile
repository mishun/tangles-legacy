Cpp := g++
CppFlags := -Wall -Wextra -pipe -std=c++0x -O4
LinkFlags := -pipe -Wl,--enable-auto-import -Wl,--stack=67108864 -O2
#CppFlags := -Wall -Wextra -pipe -std=c++0x -g
#LinkFlags := -pipe -Wl,--enable-auto-import -Wl,--stack=67108864 -g

ObjDir := obj
BinDir := bin

.PHONY: all
all: DirectoryStructure t.exe


oFileFromSrc = $(addprefix $(ObjDir)/, $(patsubst %.cpp, %.o, $(1)))
dFileFromSrc = $(addprefix $(ObjDir)/, $(patsubst %.cpp, %.d, $(1)))


srcList := t.cpp
#include ChordDiagrams/ChordDiagrams.mk
include Draw/Draw.mk
include Geometry/Geometry.mk
include Graph/Graph.mk
#include Tangles/Tangles.mk
include Topology/Topology.mk
#include OpenGL/OpenGL.mk
#include Virtual/Virtual.mk

-include $(call dFileFromSrc, $(srcList) $(call binSrcPath, $(binList)))

.PHONY: DirectoryStructure
DirectoryStructure:
	@mkdir -p $(ObjDir)
#	mkdir -p $(BinDir)
	@mkdir -p $(dir $(call oFileFromSrc, $(srcList) ) )

$(ObjDir)/%.o: %.cpp
	$(Cpp) $(CppFlags) -MMD -I . -o $@ -c $<

%.exe: $(call oFileFromSrc, $(srcList) %.cpp)
	$(Cpp) $(LinkFlags) -o $@ $^

.PHONY: clean
clean:
	rm -f $(call oFileFromSrc, $(srcList) $(call binSrcPath, $(binList)))
	rm -f $(call dFileFromSrc, $(srcList) $(call binSrcPath, $(binList)))
	rm -rf $(ObjDir)
