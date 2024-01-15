#ifndef MATRIX_H
#define MATRIX_H

#include "fixed_maths.h"
#include <stdint.h>
#include <string.h>

void mat_set_identity_4x4(RasFixed s1[4][4]);
void mat_mul_3x3_3x1(RasFixed s1[3][3], RasFixed* s2, RasFixed* dest);
void mat_mul_4x4_4x1(RasFixed s1[4][4], RasFixed* s2, RasFixed* dest);
void mat_mul_4x4_4x4(RasFixed s1[4][4], RasFixed s2[4][4], RasFixed dest[4][4]);

#endif
