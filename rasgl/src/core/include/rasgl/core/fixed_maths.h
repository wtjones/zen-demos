#ifndef FIXED_MATHS_H
#define FIXED_MATHS_H

#include <assert.h>
#include <stdint.h>

#define INT_32_TO_FIXED_16_16(n) (int32_t)((n) << 16)
#define FIXED_16_16_TO_INT_32(n) ((n) >> 16)
#define FIXED_MAX (32767 << 16)
#define FIXED_MIN (-32768 << 16)

/**
 * Multiply fixed-point by fixed-point.
 *
 * Handles underflow by rounding to smallest fractional value with the
 * same sign.
 */
static inline int32_t mul_fixed_16_16_by_fixed_16_16(int32_t f1, int32_t f2)
{
    int64_t result64 = (int64_t)f1 * (int64_t)f2;
    int32_t result = result64 >> 16;
    if (result64 != 0 && result == 0) {
        return (result64 > 0) ? 1 : -1;
    }

    return result;
}

static inline int64_t mul_fixed_16_16_to_fixed_32_32(int32_t f1, int32_t f2)
{
    int64_t result = (int64_t)f1 * (int64_t)f2;
    return result;
}

static inline int32_t div_fixed_16_16_by_fixed_16_16(int32_t f1, int32_t f2)
{
    assert((int64_t)f2 << 16 != 0);
    int64_t result = ((int64_t)f1 << 32) / ((int64_t)f2 << 16);
    return result;
}
#define RAS_FLOAT_TO_FIXED(n) \
    (int32_t) n * 65536 + ((n - (float)((int32_t)n)) * 65536)

static inline int32_t float_to_fixed_16_16(float n)
{
    int32_t whole = (int32_t)n;
    float frac = n - (float)whole;
    int32_t fixed_frac = frac * 65536;
    return whole * 65536 + fixed_frac;
}

static inline float fixed_16_16_to_float(int32_t n)
{
    int32_t whole = n / 65536;
    int32_t fixed_frac = n - (whole * 65536);
    return (float)(whole + (float)fixed_frac / 65536);
}

#endif
