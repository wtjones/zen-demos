set(CMAKE_VERBOSE_MAKEFILE ON)
add_library(external
    log.c/src/log.c)
target_include_directories(external PUBLIC . )

