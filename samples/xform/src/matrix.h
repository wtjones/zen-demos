#ifndef MATRIX_H
#define MATRIX_H

#include "fixed_maths.h"
#include <stdint.h>

void mat_mul_3x3_3x1(int32_t s1[3][3], int32_t* s2, int32_t* dest);

#endif
