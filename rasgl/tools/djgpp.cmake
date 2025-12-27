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

set(BUILD_SHARED_LIBS OFF CACHE INTERNAL "Shared libs not available" )
set(CMAKE_POSITION_INDEPENDENT_CODE OFF)
set(DJGPP ON)
