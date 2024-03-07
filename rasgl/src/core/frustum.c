#include "rasgl/core/frustum.h"
#include "rasgl/core/repr.h"

void core_frustum_plane_init(RasFixed v[4], RasPlane* plane)
{
    RasFixed length;
    plane->normal.x = v[0];
    plane->normal.y = v[1];
    plane->normal.z = v[2];

    length = core_get_vec_length(&plane->normal);
    core_normalize(&plane->normal);
    plane->distance = div_fixed_16_16_by_fixed_16_16(v[3], length);
}

/**
 *  Performs intersection tests to find the points of the frustum 'box'.
 * Points are ordered like this:
 *   the top of the frustum...
 *   0  3
 *
 *   1  2
 *
 *   bottom...
 *   4  6
 *
 *   5  7
 */
void core_frustum_points_init(RasFrustum* frustum)
{
    // top points
    core_get_3_plane_intersection(
        &frustum->planes[PLANE_TOP],
        &frustum->planes[PLANE_LEFT],
        &frustum->planes[PLANE_FAR],
        &frustum->points[0]);

    core_get_3_plane_intersection(
        &frustum->planes[PLANE_TOP],
        &frustum->planes[PLANE_LEFT],
        &frustum->planes[PLANE_NEAR],
        &frustum->points[1]);

    core_get_3_plane_intersection(
        &frustum->planes[PLANE_TOP],
        &frustum->planes[PLANE_RIGHT],
        &frustum->planes[PLANE_NEAR],
        &frustum->points[2]);

    core_get_3_plane_intersection(
        &frustum->planes[PLANE_TOP],
        &frustum->planes[PLANE_RIGHT],
        &frustum->planes[PLANE_FAR],
        &frustum->points[3]);

    // bottom points
    core_get_3_plane_intersection(
        &frustum->planes[PLANE_BOTTOM],
        &frustum->planes[PLANE_LEFT],
        &frustum->planes[PLANE_FAR],
        &frustum->points[4]);

    core_get_3_plane_intersection(
        &frustum->planes[PLANE_BOTTOM],
        &frustum->planes[PLANE_LEFT],
        &frustum->planes[PLANE_NEAR],
        &frustum->points[5]);

    core_get_3_plane_intersection(
        &frustum->planes[PLANE_BOTTOM],
        &frustum->planes[PLANE_RIGHT],
        &frustum->planes[PLANE_NEAR],
        &frustum->points[6]);

    core_get_3_plane_intersection(
        &frustum->planes[PLANE_BOTTOM],
        &frustum->planes[PLANE_RIGHT],
        &frustum->planes[PLANE_FAR],
        &frustum->points[7]);
}

void core_frustum_planes_init(RasFixed view_projection_matrix[4][4], RasFrustum* frustum)
{
    RasFixed row0[4] = {
        view_projection_matrix[0][0],
        view_projection_matrix[0][1],
        view_projection_matrix[0][2],
        view_projection_matrix[0][3]
    };

    RasFixed row1[4] = {
        view_projection_matrix[1][0],
        view_projection_matrix[1][1],
        view_projection_matrix[1][2],
        view_projection_matrix[1][3]
    };

    RasFixed row2[4] = {
        view_projection_matrix[2][0],
        view_projection_matrix[2][1],
        view_projection_matrix[2][2],
        view_projection_matrix[2][3]
    };

    RasFixed row3[4] = {
        view_projection_matrix[3][0],
        view_projection_matrix[3][1],
        view_projection_matrix[3][2],
        view_projection_matrix[3][3]
    };

    // left
    // p1 = row3 + row0
    RasFixed p1[4] = {
        row3[0] + row0[0],
        row3[1] + row0[1],
        row3[2] + row0[2],
        row3[3] + row0[3]
    };

    core_frustum_plane_init(p1, &frustum->planes[PLANE_LEFT]);

    // right
    // p2 = row3 - row0
    RasFixed p2[4] = {
        row3[0] - row0[0],
        row3[1] - row0[1],
        row3[2] - row0[2],
        row3[3] - row0[3]
    };

    core_frustum_plane_init(p2, &frustum->planes[PLANE_RIGHT]);

    // top
    // p3 = row3 + row1
    RasFixed p3[4] = {
        row3[0] + row1[0],
        row3[1] + row1[1],
        row3[2] + row1[2],
        row3[3] + row1[3]
    };

    core_frustum_plane_init(p3, &frustum->planes[PLANE_TOP]);

    // bottom
    // p4 = row3 - row1
    RasFixed p4[4] = {
        row3[0] - row1[0],
        row3[1] - row1[1],
        row3[2] - row1[2],
        row3[3] - row1[3]
    };

    core_frustum_plane_init(p4, &frustum->planes[PLANE_BOTTOM]);

    // near
    // p5 = row3 + row2
    RasFixed p5[4] = {
        row3[0] + row2[0],
        row3[1] + row2[1],
        row3[2] + row2[2],
        row3[3] + row2[3]
    };

    core_frustum_plane_init(p5, &frustum->planes[PLANE_NEAR]);

    // far
    // p6 = row3 - row2
    RasFixed p6[4] = {
        row3[0] - row2[0],
        row3[1] - row2[1],
        row3[2] - row2[2],
        row3[3] - row2[3]
    };

    core_frustum_plane_init(p6, &frustum->planes[PLANE_FAR]);
}

void core_frustum_init(RasFixed view_projection_matrix[4][4], RasFrustum* frustum)
{
    core_frustum_planes_init(view_projection_matrix, frustum);
    core_frustum_points_init(frustum);
}

bool core_point_in_frustum(RasFrustum* frustum, RasVector3f* point)
{
    bool outside_any_plane = false;
    for (RasFrustumPlane p = 0; p < 6; p++) {
        RasPlane* plane = &frustum->planes[p];

        bool outside = core_plane_vector_side(plane, point) == RAS_PLANE_SIDE_A;
        if (outside) {
            return false;
        }
    }
    return true;
}

RasClipFlags core_point_in_frustum_planes(RasFrustum* frustum, RasVector3f* point)
{
    RasClipFlags flags = 0;
    for (RasFrustumPlane p = 0; p < 6; p++) {
        RasPlane* plane = &frustum->planes[p];

        bool outside = core_plane_vector_side(plane, point) == RAS_PLANE_SIDE_A;
        flags = flags | outside << p;
    }
    return flags;
}

char* core_repr_frustum(char* buffer, size_t count, RasFrustum* frustum)
{
    char buffer2[255];
    char buffer3[255];
    char buffer4[255];

    buffer[0] = '\n';
    buffer[1] = '\0';

    snprintf(
        buffer2,
        sizeof buffer2,
        "left plane:%s\n",
        core_repr_plane(buffer3, sizeof buffer3, &frustum->planes[PLANE_LEFT]));
    strcat(buffer, buffer2);

    snprintf(
        buffer2,
        sizeof buffer2,
        "right plane:%s\n",
        core_repr_plane(buffer3, sizeof buffer3, &frustum->planes[PLANE_RIGHT]));
    strcat(buffer, buffer2);

    snprintf(
        buffer2,
        sizeof buffer2,
        "frustum points:\n    far-TL: %s\n    near-TL: %s\n",
        repr_point3f(buffer3, sizeof buffer3, &frustum->points[0]),
        repr_point3f(buffer4, sizeof buffer4, &frustum->points[1]));
    strcat(buffer, buffer2);

    return buffer;
}