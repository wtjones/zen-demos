cmake_minimum_required(VERSION 3.21)

project(
    hello
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)

set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)

add_executable(${PROJECT_NAME} src/main.c)

target_link_libraries(hello PRIVATE m PUBLIC)
