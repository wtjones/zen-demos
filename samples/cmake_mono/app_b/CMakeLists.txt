# CMakeLists.txt for app_b

cmake_minimum_required(VERSION 3.0)
project(app_b)


set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)


# Find the lib_a package
find_package(lib_a REQUIRED)


# Include directories for lib_a
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../lib_a/include)

# Add your application executable
add_executable(app_b
    src/main.c
    # Add all the source files of your application here
)

# Link your target to lib_a
target_link_libraries(app_b PRIVATE lib_a::lib_a)

# Set include directories for your application
target_include_directories(app_b PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

# Optionally, set any compiler options, etc.
# target_compile_options(app_b PRIVATE ...)

# Optionally, set any linker options, etc.
# target_link_options(app_b PRIVATE ...)
