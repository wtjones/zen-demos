set(CMAKE_VERBOSE_MAKEFILE ON)

# Create individual library targets

add_library (font8x8 INTERFACE)
target_include_directories(font8x8 INTERFACE .)

add_library(fpsqrt fpsqrt/fpsqrt.c)
target_include_directories(fpsqrt PUBLIC .)

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
target_link_libraries(external INTERFACE fpsqrt font8x8)
