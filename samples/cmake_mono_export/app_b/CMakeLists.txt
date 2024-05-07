# CMakeLists.txt for app_b

cmake_minimum_required(VERSION 3.7)
project(app_b)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

message("current src: ${CMAKE_CURRENT_SOURCE_DIR}")


find_library(lib_a_LIBRARY NAMES lib_a HINTS "${CMAKE_CURRENT_SOURCE_DIR}/../lib_a/build")

if(NOT lib_a_LIBRARY)
    message(FATAL_ERROR "lib_a library not found")
endif()

message("Value of lib_a_LIBRARY: ${lib_a_LIBRARY}")


add_executable(app_b
    src/main.c
)

target_link_libraries(app_b PRIVATE ${lib_a_LIBRARY})

target_include_directories(app_b PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/../lib_a/include")
