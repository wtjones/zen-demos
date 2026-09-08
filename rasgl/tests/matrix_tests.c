
#include "log.c/src/log.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/matrix.h"
#include "rasgl/core/repr.h"
#include "test_support.h"
#include <assert.h>

TEST(TEST_MAT_MUL)
{
    char buffer[100];
    RasFixed matrix[4][4];
    mat_set_identity_4x4(matrix);

    RasFixed src[4] = {
        float_to_fixed_16_16(3.5),
        float_to_fixed_16_16(1.2),
        float_to_fixed_16_16(5.375),
        float_to_fixed_16_16(1.0)
    };

    RasFixed dst[4];
    mat_mul_4x4_4x1(matrix, src, dst);
    ras_log_info("Result of mul by identity: %s\n", repr_mat_4x1(buffer, sizeof buffer, dst));
    return false;
}

TEST(TEST_MAT_MUL_4)
{
    char buffer[500];
    RasFixed m1[4][4] = {
        { INT_32_TO_FIXED_16_16(5), INT_32_TO_FIXED_16_16(7), INT_32_TO_FIXED_16_16(9), INT_32_TO_FIXED_16_16(10) },
        { INT_32_TO_FIXED_16_16(2), INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(8) },
        { INT_32_TO_FIXED_16_16(8), INT_32_TO_FIXED_16_16(10), INT_32_TO_FIXED_16_16(2), INT_32_TO_FIXED_16_16(3) },
        { INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(4), INT_32_TO_FIXED_16_16(8) }
    };

    RasFixed m2[4][4] = {
        { INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(10), INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(18) },
        { INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(1), INT_32_TO_FIXED_16_16(4), INT_32_TO_FIXED_16_16(9) },
        { INT_32_TO_FIXED_16_16(9), INT_32_TO_FIXED_16_16(10), INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(2) },
        { INT_32_TO_FIXED_16_16(3), INT_32_TO_FIXED_16_16(12), INT_32_TO_FIXED_16_16(4), INT_32_TO_FIXED_16_16(10) }
    };

    RasFixed dest[4][4];

    mat_mul_4x4_4x4(m1, m2, dest);
    ras_log_info("Result of 4x4 x 4x4: %s\n", repr_mat_4x4(buffer, sizeof buffer, dest));
    return false;
}
