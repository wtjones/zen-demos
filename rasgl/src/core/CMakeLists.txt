add_library(core
    frustum.c
    graphics.c
    maths.c
    maths_tables.c
    matrix.c
    plane.c
    repr.c)
target_include_directories(core PUBLIC include )
target_link_libraries(core PUBLIC external m)
