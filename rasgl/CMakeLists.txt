cmake_minimum_required(VERSION 3.7)

project(
    RasGL
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)

add_compile_definitions(DEBUG=${DEBUG})

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
option(ENABLE_ASAN "Enable AddressSanitizer" OFF)

if(ENABLE_ASAN)
  add_compile_options(-fsanitize=address -fno-omit-frame-pointer -g -fno-sanitize=leak)
  add_link_options(-fsanitize=address -fno-sanitize=leak)
endif()

add_subdirectory(src)
add_subdirectory(tests)
