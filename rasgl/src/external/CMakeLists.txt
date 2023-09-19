set(CMAKE_VERBOSE_MAKEFILE ON)
add_library(external
    log.c/src/log.c
    fpsqrt/fpsqrt.c)
target_include_directories(external PUBLIC . )
