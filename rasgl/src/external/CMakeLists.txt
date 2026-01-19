set(CMAKE_VERBOSE_MAKEFILE ON)

# Create individual library targets

add_library (font8x8 INTERFACE)
target_include_directories(font8x8 INTERFACE .)

add_library(fpsqrt fpsqrt/fpsqrt.c)
target_include_directories(fpsqrt PUBLIC .)

add_library(log_c log.c/src/log.c)
target_include_directories(log_c PUBLIC .)

# Create interface libraries for grouping

# Full libc (FILE, system() etc)
add_library(external_hosted INTERFACE)
target_link_libraries(external_hosted INTERFACE log_c)

# Minimal/embedded
add_library(external INTERFACE)
target_link_libraries(external INTERFACE fpsqrt font8x8)
