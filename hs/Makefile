
Haskell := ghc
Cpp := g++

HaskellExts := \
	MultiParamTypeClasses \
	FunctionalDependencies \
	FlexibleInstances \
	FlexibleContexts \
	BangPatterns

HaskellFlags := $(addprefix -X, $(HaskellExts)) -Wall -fspec-constr-count=9 -O2
# -funfolding-use-threshold=16
# -rtsopts -prof -auto-all -caf-all
CppFlags := -Wall -Wextra -pipe -std=c++0x -O2

Libs := stdc++


ObjDir := obj
BinDir := bin
BinExt := .exe

oFileName = $(addprefix $(ObjDir)/, $(addsuffix .o, $(1)))
dFileName = $(addprefix $(ObjDir)/, $(addsuffix .d, $(1)))


CppFiles := \
	Data/Native/CRC.cpp \
	Data/Native/BinaryBlock.cpp \
	Data/Native/DisjointSet.cpp \
	Math/KnotTh/Tangles/BorderIncremental/BITangleSkeleton.cpp \
	Math/KnotTh/Tangles/Invariants/JonesPolynomial.cpp \
	Math/KnotTh/Tangles/Invariants/RootCode.cpp \
	Math/KnotTh/Tangles/TangleSkeleton.cpp \
	Math/Numerical/LinearSystem.cpp \
	Math/Numerical/Vector2D.cpp \
	Math/Surf/Graph/Embedding.cpp \


HsFiles := \
	Algo/Lyndon.hs \
	Data/BinaryBlock.hs \
	Data/CRC.hs \
	Data/DisjointSet.hs \
	Data/EquivalenceClasses.hs \
	Data/IxWith.hs \
	Math/Algebra.hs \
	Math/Algebra/RotationDirection.hs \
	Math/Algebra/Group.hs \
	Math/Algebra/Group/Dn.hs \
	Math/Algebra/Group/D4.hs \
	Math/Algebra/Group/Sn.hs \
	Math/Algebra/Polynomial.hs \
	Math/Algebra/Ring.hs \
	Math/Combinatorics/ChordDiagrams.hs \
	Math/Combinatorics/ChordDiagrams/ChordDiagramSt.hs \
	Math/Combinatorics/ChordDiagrams/Generator.hs \
	Math/KnotTh/Crossings/ProjectionCrossing.hs \
	Math/KnotTh/Crossings/ArbitraryCrossing.hs \
	Math/KnotTh/Crossings/SubTangleCrossing.hs \
	Math/KnotTh/Tangles/BorderIncremental/BITangle.hs \
	Math/KnotTh/Tangles/BorderIncremental/BITangleSkeleton.hs \
	Math/KnotTh/Tangles/BorderIncremental/Generator.hs \
	Math/KnotTh/Tangles/BorderIncremental/GlueSites.hs \
	Math/KnotTh/Tangles/BorderIncremental/Tests.hs \
	Math/KnotTh/Tangles/Invariants/InvariantsSet.hs \
	Math/KnotTh/Tangles/Invariants/JonesPolynomial.hs \
	Math/KnotTh/Tangles/Invariants/LinkingNumber.hs \
	Math/KnotTh/Tangles/Invariants/RootCode.hs \
	Math/KnotTh/Tangles/Invariants/ThreadExpansion.hs \
	Math/KnotTh/Tangles/Moves/DoublePass.hs \
	Math/KnotTh/Tangles/Moves/Flype.hs \
	Math/KnotTh/Tangles/Moves/Moves.hs \
	Math/KnotTh/Tangles/Moves/Pass.hs \
	Math/KnotTh/Tangles/Moves/ReidemeisterIII.hs \
	Math/KnotTh/Tangles/Moves/ReidemeisterReduction.hs \
	Math/KnotTh/Tangles/Moves/Weak.hs \
	Math/KnotTh/Tangles/Util/Alternating.hs \
	Math/KnotTh/Tangles/Util/Arbitrary.hs \
	Math/KnotTh/Tangles/Util/Connectivity.hs \
	Math/KnotTh/Tangles/Util/Draw.hs \
	Math/KnotTh/Tangles/Util/Embedding.hs \
	Math/KnotTh/Tangles/Util/List.hs \
	Math/KnotTh/Tangles/Util/Paths.hs \
	Math/KnotTh/Tangles/Util/Projection.hs \
	Math/KnotTh/Tangles/Util/Resting.hs \
	Math/KnotTh/Tangles/Util/Show.hs \
	Math/KnotTh/Tangles/Util/Writhe.hs \
	Math/KnotTh/Tangles/ArbitraryGenerator.hs \
	Math/KnotTh/Tangles/FlypeGenerator.hs \
	Math/KnotTh/Tangles/TangleSkeleton.hs \
	Math/KnotTh/Tangles/TangleSt.hs \
	Math/KnotTh/Tangles/Util.hs \
	Math/KnotTh/Tangles.hs \
	Math/Numerical/GradientDescent.hs \
	Math/Numerical/LinearSystem.hs \
	Math/Surf/Graph.hs \
	Math/Surf/Graph/Construction.hs \
	Math/Surf/Graph/Dart.hs \
	Math/Surf/Graph/Embedding.hs \
	Math/Surf/Graph/Face.hs \
	Math/Surf/Graph/GraphSt.hs \
	Math/Surf/Graph/Util.hs \
	Math/Surf/Graph/Vertex.hs \
	Math/KnotTh.hs \
	Graph.hs \
	Graph/BoundingBox.hs \
	Graph/Color.hs \
	Graph/DrawContext.hs \
	Graph/Image.hs \
	Graph/ImageBody.hs \
	Graph/MetaPost.hs \
	Graph/Path.hs \
	Graph/PostScript.hs \
	Graph/PostScript/Context.hs \
	Graph/PostScript/Path.hs \
	Graph/PostScript/Transform.hs \
	Graph/Transform.hs \
	Graph/Units.hs \


BinFiles := \
	Main \
	CD \


.PHONY: all
all: DirectoryStructure $(addprefix $(BinDir)/, $(addsuffix $(BinExt), $(BinFiles)))

.PHONY: profile
profile: $(BinDir)/CD.ps
#$(addsuffix .ps, $(BinFiles))

.PHONY: DirectoryStructure
DirectoryStructure:
	@mkdir -p $(ObjDir) $(BinDir)

$(BinDir)/%$(BinExt): %.hs $(HsFiles) $(call oFileName, $(CppFiles))
	$(Haskell) --make -osuf=hs.o -outputdir=$(ObjDir) $(HaskellFlags) -o $@ $< $(addprefix -l, $(Libs)) $(filter %.o, $^)

.SECONDARY:
$(ObjDir)/%.cpp.o: %.cpp
	@mkdir -p $(dir $@)
	$(Cpp) $(CppFlags) -I $(CURDIR) -MMD -o $@ -c $<

$(BinDir)/%.ps: $(BinDir)/%.hp
	hp2ps -c $<

$(BinDir)/%.hp: $(BinDir)/%$(BinExt)
	$< +RTS -p -sstderr -hy

.PHONY: clean
clean:
	rm -rf $(ObjDir)

-include $(call dFileName, $(CppFiles))
