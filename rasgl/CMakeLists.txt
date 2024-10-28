cmake_minimum_required(VERSION 3.7)

project(
    RasGL
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)

add_compile_definitions(DEBUG=${DEBUG})

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_subdirectory(src)
if (NOT DEFINED RAS_PLATFORM)
    add_subdirectory(tests)
endif()
