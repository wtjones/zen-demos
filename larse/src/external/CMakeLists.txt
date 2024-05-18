set(CMAKE_VERBOSE_MAKEFILE ON)
add_library(external_unity STATIC
    unity/unity.c)
target_include_directories(external_unity PUBLIC unity )
