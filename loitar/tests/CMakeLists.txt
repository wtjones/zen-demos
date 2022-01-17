# Testing library
FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG v2.13.7)
FetchContent_MakeAvailable(Catch2)
# Adds Catch2::Catch2

#Tests need to be added as executables first
add_executable(testlib
    testlib.cpp
    environment_tests.cpp
    evaluator_tests.cpp
    node_tests.cpp
    parser_tests.cpp
    syslib/math_ops_tests.cpp)

target_compile_features(testlib PRIVATE cxx_std_17)

# Should be linked to the main library, as well as the Catch2 testing library
target_link_libraries(testlib PRIVATE loitar_core PRIVATE Catch2::Catch2 spdlog::spdlog)

target_include_directories(testlib PUBLIC ../src/core/include)

list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/contrib)
include(Catch)
catch_discover_tests(testlib)
