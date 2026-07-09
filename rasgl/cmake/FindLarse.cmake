# Include larse, a library in the mono-repo.
# A better approach would be to use something like FetchContent or find_package

if(RAS_PLATFORM STREQUAL "ras_dos")
  set(LARSE_BUILD_DIR ${CMAKE_SOURCE_DIR}/../larse/bld_dos)
else()
  set(LARSE_BUILD_DIR ${CMAKE_SOURCE_DIR}/../larse/build)
endif()

set(LARSE_DIR ${LARSE_BUILD_DIR}/src/core)
set(LARSE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/../larse/src/core/include)

add_library(larse::core STATIC IMPORTED)

set_target_properties(larse::core PROPERTIES
    IMPORTED_LOCATION "${LARSE_DIR}/liblarse_core.a"
    INTERFACE_INCLUDE_DIRECTORIES "${LARSE_INCLUDE_DIR}"
    INTERFACE_LINK_LIBRARIES "m"
)
