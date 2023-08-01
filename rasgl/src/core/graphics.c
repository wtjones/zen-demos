#include "rasgl/core/graphics.h"
#include "rasgl/core/debug.h"

/**
 * project view point to screen
 *
 */
Point2i project_point(int32_t screen_width, int32_t screen_height, int32_t projection_ratio, Point3f view_point)
{
    char buffer[100];

    Point2i result;
    debug_print("view_point: %s\n", repr_point3f(buffer, sizeof buffer, &view_point));

    // x = ((x / z * proj_ratio * (SCREEN_W / 2.0) + 0.5)) + SCREEN_W / 2

    int32_t x_div_z
        = div_fixed_16_16_by_fixed_16_16(view_point.x, view_point.z);
    debug_print("x_div_z = %s\n", repr_fixed_16_16(buffer, sizeof buffer, x_div_z));

    int32_t mul_ratio = mul_fixed_16_16_by_fixed_16_16(x_div_z, projection_ratio);
    debug_print("mul_ratio = %s\n", repr_fixed_16_16(buffer, sizeof buffer, mul_ratio));

    int32_t screen_div_2 = INT_32_TO_FIXED_16_16(screen_width / 2);
    debug_print("screen_div2 = %s\n", repr_fixed_16_16(buffer, sizeof buffer, screen_div_2));
    int32_t left = mul_fixed_16_16_by_fixed_16_16(mul_ratio, screen_div_2);
    debug_print("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));
    left += float_to_fixed_16_16(0.5);
    debug_print("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));

    int32_t right = INT_32_TO_FIXED_16_16(screen_width / 2);
    result.x = left + right;
    debug_print("result.x = %s\n", repr_fixed_16_16(buffer, sizeof buffer, result.x));

    // y = ((y / z * -1.0 * proj_ratio * (SCREEN_W / 2.0) + 0.5)) + SCREEN_H / 2
    int32_t y_div_z
        = div_fixed_16_16_by_fixed_16_16(view_point.y, view_point.z);
    debug_print("y_div_z = %s\n", repr_fixed_16_16(buffer, sizeof buffer, y_div_z));

    mul_ratio = mul_fixed_16_16_by_fixed_16_16(y_div_z, projection_ratio);
    mul_ratio = mul_fixed_16_16_by_fixed_16_16(mul_ratio, -float_to_fixed_16_16(1.0));

    debug_print("mul_ratio = %s\n", repr_fixed_16_16(buffer, sizeof buffer, mul_ratio));

    screen_div_2 = INT_32_TO_FIXED_16_16(screen_width / 2);
    debug_print("screen_div2 = %s\n", repr_fixed_16_16(buffer, sizeof buffer, screen_div_2));
    left = mul_fixed_16_16_by_fixed_16_16(mul_ratio, screen_div_2);
    debug_print("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));
    left += float_to_fixed_16_16(0.5);
    debug_print("left = %s\n", repr_fixed_16_16(buffer, sizeof buffer, left));

    right = INT_32_TO_FIXED_16_16(screen_height / 2);
    result.y = left + right;
    debug_print("result.y = %s\n", repr_fixed_16_16(buffer, sizeof buffer, result.x));

    return result;
}
