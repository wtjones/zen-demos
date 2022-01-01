cmake_minimum_required(VERSION 3.21)

project(
    Loitar
    VERSION 0.1
    DESCRIPTION "An experiment in Lisp"
    LANGUAGES CXX)

include(FetchContent)

FetchContent_Declare(
    spdlog
    GIT_REPOSITORY https://github.com/gabime/spdlog.git
    GIT_TAG v1.9.1)
    FetchContent_MakeAvailable(spdlog)

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_subdirectory(src)

include(CTest)

add_subdirectory(tests)
