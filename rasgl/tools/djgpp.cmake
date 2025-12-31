# Example
#
# export DJGPP_PREFIX="/usr/local/djgpp"
# source $DJGPP_PREFIX/setenv
# cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -B build

set(CMAKE_C_COMPILER "i586-pc-msdosdjgpp-gcc")
set(CMAKE_CXX_COMPILER "i586-pc-msdosdjgpp-g++")
set(CMAKE_AR "i586-pc-msdosdjgpp-ar")

set(CMAKE_RANLIB "i586-pc-msdosdjgpp-ranlib")
set(CMAKE_SYSTEM_PROCESSOR i586)
set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_CROSSCOMPILING ON)
set(CMAKE_SHARED_LIBRARY_LINK_C_FLAGS "")
set(CMAKE_C_LINK_FLAGS "")

# Fix for Allegro 4.2 inline function issues with modern GCC
# https://www.allegro.cc/forums/thread/617748
add_compile_options(-fgnu89-inline)
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# Release-specific optimizations
# -O2: Good balance of speed and binary size (use -O3 for max speed, -Os for size)
# -fomit-frame-pointer: Free up a register on x86
# -march=i586: Optimize for Pentium (use i486 for 486-class CPUs)
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    add_compile_options(-O2 -fomit-frame-pointer -march=i586)
endif()

set(BUILD_SHARED_LIBS OFF CACHE INTERNAL "Shared libs not available" )
set(CMAKE_POSITION_INDEPENDENT_CODE OFF)
set(DJGPP ON)
