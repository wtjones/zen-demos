
include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)

add_library(
    hosted
    event_log.c
    log_impl.c
    sc_load.c
    sc_gridmap_load.c
    sc_tombmap_load.c)

target_include_directories(hosted PUBLIC include)
target_link_libraries(hosted PUBLIC external_hosted m core ${LARSE_LIB})
