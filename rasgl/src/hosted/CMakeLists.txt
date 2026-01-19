
include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)

add_library(hosted
    log_impl.c)

target_include_directories(hosted PUBLIC include)
target_link_libraries(hosted PUBLIC external_hosted m core ${LARSE_LIB})
