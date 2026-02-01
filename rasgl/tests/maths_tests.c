
#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"
#include "tests.h"
#include <math.h>

void fov_scale_tests()
{
    float d2r = M_PI / 180.0f;
    for (float fov = 30.0f; fov <= 120.0f; fov += 1.0f) {
        float y_scale = 1.0f / tanf(d2r * fov / 2);
        ras_log_info("FOV, y scale %f, %f %d", fov, y_scale, float_to_fixed_16_16(y_scale));
    }
}
