# CMakeLists.txt for lib_a

cmake_minimum_required(VERSION 3.0)
project(lib_a)


set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)

# Define your library
add_library(lib_a SHARED
    src/parse.c
)

# Set include directories
target_include_directories(lib_a
    PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Optionally, set any compiler options, etc.
# target_compile_options(lib_a PRIVATE ...)

# Optionally, set any linker options, etc.
# target_link_options(lib_a PRIVATE ...)

# Generate the Config.cmake file
include(CMakePackageConfigHelpers)
configure_package_config_file(
    ${CMAKE_CURRENT_LIST_DIR}/cmake/lib_aConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/lib_aConfig.cmake
    INSTALL_DESTINATION lib/cmake/lib_a
)

# Install the library and its headers
install(TARGETS lib_a
    EXPORT lib_aTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include/
    DESTINATION include
)

# Install the Config.cmake file
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib_aConfig.cmake
    DESTINATION lib/cmake/lib_a
)

# Install the exported targets file
install(EXPORT lib_aTargets
    FILE lib_aTargets.cmake
    NAMESPACE lib_a::
    DESTINATION lib/cmake/lib_a
)
