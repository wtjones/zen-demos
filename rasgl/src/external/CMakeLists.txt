set(CMAKE_VERBOSE_MAKEFILE ON)

# Create individual library targets

add_library (font8x8 INTERFACE)
target_include_directories(font8x8 INTERFACE .)

add_library(fpsqrt fpsqrt/fpsqrt.c)
target_include_directories(fpsqrt PUBLIC .)

add_library(mpack
    mpack/src/mpack-common.c
    mpack/src/mpack-expect.c
    mpack/src/mpack-writer.c
    mpack/src/mpack-reader.c
    mpack/src/mpack-node.c
    mpack/src/mpack-platform.c)
target_include_directories(mpack PUBLIC .)
target_compile_definitions(mpack PUBLIC
    MPACK_CONFORMING=1  # PSX uses shim inttypes.h
    MPACK_STDIO=0       # PSX doesn't have errno.h
)

if (NOT RAS_PLATFORM_TYPE STREQUAL "embedded")
    add_library(log_c log.c/src/log.c)
    target_include_directories(log_c PUBLIC .)
endif()

# Create interface libraries for grouping

if (NOT RAS_PLATFORM_TYPE STREQUAL "embedded")
    # Full libc (FILE, system() etc)
    add_library(external_hosted INTERFACE)
    target_link_libraries(external_hosted INTERFACE log_c)
endif()

# Minimal/embedded
add_library(external INTERFACE)
target_link_libraries(external INTERFACE fpsqrt font8x8 mpack)
