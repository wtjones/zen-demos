# Include larse, a library in the mono-repo.
# A better approach would be to use something like FetchContent or find_package

set(LARSE_DIR ${CMAKE_SOURCE_DIR}/../larse/build/src/core)
set(LARSE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/../larse/src/core/include)
set(LARSE_LIB ${LARSE_DIR}/liblarse_core.a)

message("LARSE_DIR: ${LARSE_DIR}")
message("LARSE_INCLUDE_DIR: ${LARSE_INCLUDE_DIR}")
message("LARSE_LIB: ${LARSE_LIB}")

include_directories(${LARSE_INCLUDE_DIR})