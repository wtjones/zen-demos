cmake_minimum_required(VERSION 3.7)

project(
    Larse
    VERSION 0.1
    DESCRIPTION "Lisp-like language for scripting and configuration"
    LANGUAGES C)

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)


set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)

option(ENABLE_ASAN "Enable AddressSanitizer" OFF)

if(ENABLE_ASAN)
  add_compile_options(-fsanitize=address -fno-omit-frame-pointer -g -fno-sanitize=leak)
  add_link_options(-fsanitize=address -fno-sanitize=leak)
endif()

add_subdirectory(src)

include(CTest)
add_subdirectory(tests)

