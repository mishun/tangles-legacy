
ChordDiagrams_CPP := $(wildcard ChordDiagrams/*.cpp)
srcList := $(srcList) $(ChordDiagrams_CPP)

.PHONY: ChordDiagrams
ChordDiagrams: Util ChordDiagrams/ChordDiagrams.mk $(call oFileFromSrc, $(ChordDiagrams_CPP))
