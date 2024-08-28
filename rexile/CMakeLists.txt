cmake_minimum_required(VERSION 3.21)
project(rexile VERSION 1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)

find_package(PkgConfig REQUIRED)
pkg_check_modules(NCURSESW REQUIRED ncursesw)

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


 add_subdirectory(src)



include(CTest)
add_subdirectory(tests)
