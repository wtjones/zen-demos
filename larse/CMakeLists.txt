cmake_minimum_required(VERSION 3.7)

project(
    Larse
    VERSION 0.1
    DESCRIPTION "Lisp-like language for scripting and configuration"
    LANGUAGES C)


set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)

# set(TARGET_GROUP default CACHE STRING "Group to build")

# if(TARGET_GROUP STREQUAL default)

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

#     add_subdirectory(src)
# elseif(TARGET_GROUP STREQUAL test)
    include(CTest)

    add_subdirectory(src)
    add_subdirectory(tests)
#endif()

