add_library(core
    app.c
    frustum.c
    graphics.c
    input.c
    maths.c
    maths_tables.c
    matrix.c
    matrix_projection.c
    model.c
    plane.c
    repr.c)
target_include_directories(core PUBLIC include )
target_link_libraries(core PUBLIC external m)
