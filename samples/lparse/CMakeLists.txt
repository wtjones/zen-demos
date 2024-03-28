cmake_minimum_required(VERSION 3.21)

project(
    lparse
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)

include(CTest)

set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)

add_executable(${PROJECT_NAME}
    src/main.c)

target_link_libraries(lparse PRIVATE m PUBLIC)

target_include_directories(lparse PUBLIC ${SDL2_INCLUDE_DIRS})
