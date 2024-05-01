# CMakeLists.txt for lib_a

cmake_minimum_required(VERSION 3.0)
project(lib_a)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

add_library(lib_a SHARED
    src/parse.c
)

target_include_directories(lib_a
    PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

export(
    TARGETS ${CMAKE_PROJECT_NAME}
    FILE ${CMAKE_PROJECT_NAME}Targets.cmake
)
