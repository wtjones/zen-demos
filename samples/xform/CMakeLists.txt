cmake_minimum_required(VERSION 3.21)

project(
    xform
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)

include(CTest)

set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)
find_package(SDL2 REQUIRED)

add_executable(${PROJECT_NAME} src/main.c src/matrix.c src/maths.c)

target_link_libraries(xform PRIVATE m PUBLIC SDL2::SDL2)

target_include_directories(xform PUBLIC ${SDL2_INCLUDE_DIRS})

add_executable(maths-test src/test.c)

enable_testing()

add_test(
    NAME    maths-test
    COMMAND $<TARGET_FILE:maths-test>)
