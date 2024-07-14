set(CMAKE_VERBOSE_MAKEFILE ON)
add_library(external
    log.c/src/log.c)
target_include_directories(external PUBLIC . )

# Avoid outputting the absolute path of the source file in the log
target_compile_definitions(external PRIVATE "-D__FILE__=__FILE_NAME__")
