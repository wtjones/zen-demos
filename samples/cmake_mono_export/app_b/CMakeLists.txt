# CMakeLists.txt for app_b

cmake_minimum_required(VERSION 3.7)
project(app_b)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

message("current src: ${CMAKE_CURRENT_SOURCE_DIR}")

cmake_path(SET lib_a_BIN NORMALIZE "${CMAKE_CURRENT_SOURCE_DIR}/../lib_a/build/liblib_a.so")
message("Value of lib_a_BIN: ${lib_a_BIN}")

add_executable(app_b
    src/main.c
)

add_library(lib_a SHARED IMPORTED)
set_property(TARGET lib_a PROPERTY
    IMPORTED_LOCATION ${lib_a_BIN})
             
target_link_libraries(app_b PRIVATE lib_a)

target_include_directories(app_b PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib_a/include)
