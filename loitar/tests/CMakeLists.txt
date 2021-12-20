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
    evaluator_tests.cpp
    node_tests.cpp)

# I'm using C++17 in the test
target_compile_features(testlib PRIVATE cxx_std_17)

# Should be linked to the main library, as well as the Catch2 testing library
target_link_libraries(testlib PRIVATE loitar_core Catch2::Catch2 spdlog::spdlog)
#
target_include_directories(testlib PUBLIC ../src/core/include)

# If you register a test, then ctest and make test will run it.
# You can also run examples and check the output, as well.
add_test(NAME testlibtest COMMAND testlib) # Command can be a target

#list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/contrib)
#include(Catch)
#catch_discover_tests(testlib)
