
include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)

add_library(
    hosted
    event_log.c
    log_impl.c
    model_load.c
    sc_load.c
    sc_gridmap_load.c
    sc_tombmap_load.c
    string.c
)

target_include_directories(hosted PUBLIC include)
target_link_libraries(hosted PUBLIC external_hosted m core ${LARSE_LIB})
