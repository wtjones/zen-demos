cmake_minimum_required(VERSION 3.7)

project(
    RasGL
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)

    set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_subdirectory(src/platform)
    