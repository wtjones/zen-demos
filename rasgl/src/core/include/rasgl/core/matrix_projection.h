#ifndef MATRIX_PROJECTION_H
#define MATRIX_PROJECTION_H

#include "fixed_maths.h"
#include <stdint.h>

void mat_ortho_init(
    int32_t matrix[4][4],
    int32_t l,
    int32_t r,
    int32_t b,
    int32_t t,
    int32_t n,
    int32_t f);

#endif
