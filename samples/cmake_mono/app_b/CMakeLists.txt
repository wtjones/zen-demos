# CMakeLists.txt for app_b

cmake_minimum_required(VERSION 3.7)
project(app_b)


set(CMAKE_C_STANDARD 17 CMAKE_C_STANDARD_REQUIRED ON)



# Specify the path to the build directory of lib_a
set(LIB_A_DIR ../lib_a/build)
set(LIB_A_INCLUDE_DIR ../lib_a/include)


# Include lib_a headers
include_directories(${LIB_A_INCLUDE_DIR})

# Link against the static library of lib_a
link_directories(${LIB_A_DIR})


# Add your application executable
add_executable(app_b
    src/main.c
    # Add all the source files of your application here
)


# Link lib_a to app_b
target_link_libraries(app_b lib_a)
