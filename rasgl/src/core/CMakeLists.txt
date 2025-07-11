
include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)

add_library(core
    app.c
    camera.c
    console.c
    debug.c
    frustum.c
    graphics.c
    grid.c
    input.c
    line_buffer.c
    maths.c
    maths_tables.c
    matrix.c
    matrix_projection.c
    model.c
    plane.c
    rasterize.c
    repr.c
    sc_animation.c
    sc_load.c
    sc_scene.c
    text.c
    string.c)

target_include_directories(core PUBLIC include)
target_link_libraries(core PUBLIC external m ${LARSE_LIB})
