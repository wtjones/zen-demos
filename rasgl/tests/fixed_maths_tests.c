
#include "rasgl/core/debug.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/repr.h"
#include "test_support.h"
#include <math.h>

void fixed_mul_test(RasFixed f1, RasFixed f2, RasFixed expected)
{
    char buffer1[255];
    char buffer2[255];
    char buffer3[255];
    RasFixed result = mul_fixed_16_16_by_fixed_16_16(f1, f2);
    ras_log_trace(
        "%s * %s = %s\n",
        repr_fixed_16_16(buffer1, sizeof buffer1, f1),
        repr_fixed_16_16(buffer2, sizeof buffer3, f2),
        repr_fixed_16_16(buffer3, sizeof buffer3, result));

    assert(result == expected);
}

TEST(TEST_FIXED_MUL)
{
    ras_log_info("fixed_mul_tests:\n");

    fixed_mul_test(
        float_to_fixed_16_16(0.125),
        float_to_fixed_16_16(2.5),
        20480);
    return false;
}

void fixed_div_test(RasFixed f1, RasFixed f2, RasFixed expected)
{
    char buffer1[255];
    char buffer2[255];
    char buffer3[255];
    RasFixed result = div_fixed_16_16_by_fixed_16_16(f1, f2);
    ras_log_trace(
        "%s / %s = %s\n",
        repr_fixed_16_16(buffer1, sizeof buffer1, f1),
        repr_fixed_16_16(buffer2, sizeof buffer3, f2),
        repr_fixed_16_16(buffer3, sizeof buffer3, result));

    assert(result == expected);
}

TEST(TEST_FIXED_DIV)
{
    ras_log_info("fixed_div_tests:\n");
    fixed_div_test(
        float_to_fixed_16_16(34.5),
        float_to_fixed_16_16(0.125),
        18087936); // 276.0
    fixed_div_test(
        float_to_fixed_16_16(10.4),
        float_to_fixed_16_16(2.5),
        272629); // 4.16
    return false;
}
