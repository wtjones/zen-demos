
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

static inline void log_fx(float f, RasFixed fx16, RasFixed_20_12 fx20)
{
    ras_log_info("Float: %f\nFixed16: %d, %f\nFixed20: %d, %f",
        f,
        fx16,
        fixed_16_16_to_float(fx16),
        fx20,
        fixed_20_12_to_float(fx20));
}

static inline bool fixed_20_validate(float f)
{
    RasFixed_20_12 fx20 = float_to_fixed_20_12(f);
    RasFixed fx16 = float_to_fixed_16_16(f);

    log_fx(f, fx16, fx20);

    RasFixed new16 = RAS_FIXED_20_12_TO_16_16(fx20);
    RasFixed_20_12 new20 = RAS_FIXED_16_16_TO_20_12(fx16);

    log_fx(f, new16, new20);
    return (fx16 == new16) && (fx20 == new20);
}

void fixed_20_tests()
{
    assert(fixed_20_validate(1.0f));

    // Won't actually match due to rounding.
    fixed_20_validate(3.1459f);
}
