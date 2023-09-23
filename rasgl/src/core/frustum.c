#include "rasgl/core/frustum.h"

void core_frustum_plane_init(int32_t v[4], Plane* plane)
{
    int32_t length;
    plane->normal.x = v[0];
    plane->normal.y = v[1];
    plane->normal.z = v[2];

    length = core_get_vec_length(&plane->normal);
    core_normalize(&plane->normal);
    plane->distance = div_fixed_16_16_by_fixed_16_16(v[3], length);
}

void core_frustum_init(int32_t view_projection_matrix[4][4], Frustum* frustum)
{
    int32_t row0[4] = {
        view_projection_matrix[0][0],
        view_projection_matrix[0][1],
        view_projection_matrix[0][2],
        view_projection_matrix[0][3]
    };

    int32_t row1[4] = {
        view_projection_matrix[1][0],
        view_projection_matrix[1][1],
        view_projection_matrix[1][2],
        view_projection_matrix[1][3]
    };

    int32_t row2[4] = {
        view_projection_matrix[2][0],
        view_projection_matrix[2][1],
        view_projection_matrix[2][2],
        view_projection_matrix[2][3]
    };

    int32_t row3[4] = {
        view_projection_matrix[3][0],
        view_projection_matrix[3][1],
        view_projection_matrix[3][2],
        view_projection_matrix[3][3]
    };

    // left
    // p1 = row3 + row0
    int32_t p1[4] = {
        row3[0] + row0[0],
        row3[1] + row0[1],
        row3[2] + row0[2],
        row3[3] + row0[3]
    };

    core_frustum_plane_init(p1, &frustum->planes[PLANE_LEFT]);

    // right
    // p2 = row3 - row0
    int32_t p2[4] = {
        row3[0] - row0[0],
        row3[1] - row0[1],
        row3[2] - row0[2],
        row3[3] - row0[3]
    };

    core_frustum_plane_init(p2, &frustum->planes[PLANE_RIGHT]);

    // bottom
    // p3 = row3 + row1
    int32_t p3[4] = {
        row3[0] + row1[0],
        row3[1] + row1[1],
        row3[2] + row1[2],
        row3[3] + row1[3]
    };

    core_frustum_plane_init(p3, &frustum->planes[PLANE_BOTTOM]);

    // top
    // p4 = row3 - row1
    int32_t p4[4] = {
        row3[0] - row1[0],
        row3[1] - row1[1],
        row3[2] - row1[2],
        row3[3] - row1[3]
    };

    core_frustum_plane_init(p4, &frustum->planes[PLANE_TOP]);

    // near
    // p5 = row3 + row2
    int32_t p5[4] = {
        row3[0] + row2[0],
        row3[1] + row2[1],
        row3[2] + row2[2],
        row3[3] + row2[3]
    };

    core_frustum_plane_init(p5, &frustum->planes[PLANE_NEAR]);

    // far
    // p6 = row3 - row2
    int32_t p6[4] = {
        row3[0] - row2[0],
        row3[1] - row2[1],
        row3[2] - row2[2],
        row3[3] - row2[3]
    };

    core_frustum_plane_init(p6, &frustum->planes[PLANE_FAR]);
}
