
# Include larse, a library in the mono-repo.
# A better approach would be to use something like FetchContent or find_package

set(LARSE_DIR ${CMAKE_SOURCE_DIR}/../larse/build/src/core)
set(LARSE_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/../larse/src/core/include)

message("LARSE_DIR: ${LARSE_DIR}")
message("LARSE_INCLUDE_DIR: ${LARSE_INCLUDE_DIR}")

include_directories(${LARSE_INCLUDE_DIR})
link_directories(${LARSE_DIR})

add_library(core
    app.c
    camera.c
    debug.c
    frustum.c
    graphics.c
    input.c
    maths.c
    maths_tables.c
    matrix.c
    matrix_projection.c
    model.c
    plane.c
    repr.c
    scene.c
    string.c)

target_include_directories(core PUBLIC include )
target_link_libraries(core PUBLIC external m PRIVATE ${LARSE_DIR}/liblarse_core.a)
