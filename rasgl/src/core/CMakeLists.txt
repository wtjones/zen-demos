
add_library(core
    app.c
    camera.c
    clip.c
    console.c
    debug.c
    event.c
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
    normals.c
    pipeline.c
    plane.c
    rasterize.c
    repr.c
    sg_setup.c
    sg_tombmap.c
    sc_animation.c
    sc_gridmap.c
    sc_scene.c
    sc_tombmap.c
    sc_tombmap_data.c
    text.c
    string.c)

target_include_directories(core PUBLIC include)
target_link_libraries(core PUBLIC external)
