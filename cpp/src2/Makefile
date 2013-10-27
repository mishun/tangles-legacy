
CPP := g++
CPPFLAGS := -Wall -pipe -std=c++0x -O2
LNKFLAGS := -Wl,--enable-auto-import -Wl,--stack=67108864 -O2
#LIBS := -lopengl32 -lglu32 -lglut32

BinDirectory := bin
ObjDirectory := obj

oFileFromSrc = $(addprefix $(ObjDirectory)/, $(patsubst %.cpp, %.o, $(1)))
dFileFromSrc = $(addprefix $(ObjDirectory)/, $(patsubst %.cpp, %.d, $(1)))
binExePath = $(addprefix $(BinDirectory)/, $(addsuffix .exe, $(1)))
binSrcPath = $(addsuffix .cpp, $(1))

binList := main TangleGenerator CDGenerator virtual incremental

.PHONY: all
all: $(call binExePath, $(binList))

.PHONY: kino
kino: $(BinDirectory)/kino.exe

$(BinDirectory)/kino.exe: $(call oFileFromSrc, $(wildcard $(addsuffix /*.cpp, Kino Kino/Algebra Kino/GL)))

srcList := 

include ChordDiagrams/ChordDiagrams.mk
include Draw/Draw.mk
include Geometry/Geometry.mk
include Graph/Graph.mk
include Tangles/Tangles.mk
include Topology/Topology.mk
include Util/Util.mk
#include OpenGL/OpenGL.mk
include Virtual/Virtual.mk

-include $(call dFileFromSrc, $(srcList) $(call binSrcPath, $(binList)))

$(ObjDirectory)/%.o: %.cpp
	mkdir -p $(dir $@)
	$(CPP) $(CPPFLAGS) -MMD -I $(CURDIR) -c $< -o $@

$(BinDirectory)/%.exe: $(call oFileFromSrc, $(srcList) %.cpp)
	mkdir -p $(dir $@)
	$(CPP) $^ -o $@ $(LIBS) $(LNKFLAGS)

.PHONY: clean
clean:
	rm -f $(call oFileFromSrc, $(srcList) $(call binSrcPath, $(binList)))
	rm -f $(call dFileFromSrc, $(srcList) $(call binSrcPath, $(binList)))

.PHONY: delete
delete: clean
	rm -f $(call binExePath, $(binList))
