#undef NDEBUG

#include "fixed_maths.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    {
        int32_t test = float_to_fixed_16_16(2.5);
        int32_t result = mul_fixed_16_16_by_fixed_16_16(test, test);

        // 2.5 * 2.5 = 6.125
        assert(result == 409600);
    }

    {
        int64_t result = mul_fixed_16_16_to_fixed_32_32(39321600, 13085012);
        printf("%d", result);
        assert(result == 514523607859200);
    }

    return EXIT_SUCCESS;
}
