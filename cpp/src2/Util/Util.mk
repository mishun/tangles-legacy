
Util_CPP = $(wildcard Util/*.cpp)
srcList := $(srcList) $(Util_CPP)

.PHONY: Util
Util: Util/Util.mk $(call oFileFromSrc, $(Util_CPP))
