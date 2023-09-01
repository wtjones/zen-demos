cmake_minimum_required(VERSION 3.21)

project(
    sand
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)

include(CTest)

set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)
find_package(SDL2 REQUIRED)

add_executable(${PROJECT_NAME}
    src/main.c)

target_link_libraries(sand PRIVATE m PUBLIC SDL2::SDL2)

target_include_directories(sand PUBLIC ${SDL2_INCLUDE_DIRS})

add_executable(sand-test src/test.c)

enable_testing()

add_test(
    NAME    sand-test
    COMMAND $<TARGET_FILE:sand-test>)
