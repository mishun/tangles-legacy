
OpenGL_CPP = $(wildcard OpenGL/*.cpp)
srcList := $(srcList) $(OpenGL_CPP)

.PHONY: OpenGL
OpenGL: $(OpenGL_PATH)/OpenGL.mk $(call oFileFromSrc, $(OpenGL_CPP))
