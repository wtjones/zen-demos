# Include larse, a library in the mono-repo.
# A better approach would be to use something like FetchContent or find_package

if(RAS_PLATFORM STREQUAL "ras_dos")
  set(LARSE_BUILD_DIR ${CMAKE_SOURCE_DIR}/../larse/bld_dos)
else()
  set(LARSE_BUILD_DIR ${CMAKE_SOURCE_DIR}/../larse/build)
endif()

set(LARSE_DIR ${LARSE_BUILD_DIR}/src/core)
set(LARSE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/../larse/src/core/include)
set(LARSE_LIB ${LARSE_DIR}/liblarse_core.a)

message("LARSE_DIR: ${LARSE_DIR}")
message("LARSE_INCLUDE_DIR: ${LARSE_INCLUDE_DIR}")
message("LARSE_LIB: ${LARSE_LIB}")

include_directories(${LARSE_INCLUDE_DIR})
