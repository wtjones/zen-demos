# CMakeLists.txt for lib_a

cmake_minimum_required(VERSION 3.0)
project(lib_a)


set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)

# Define your library
add_library(lib_a STATIC
    src/parse.c
)

# Set include directories
target_include_directories(lib_a
    PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

# Optionally, set any compiler options, etc.
# target_compile_options(lib_a PRIVATE ...)

# Optionally, set any linker options, etc.
# target_link_options(lib_a PRIVATE ...)
