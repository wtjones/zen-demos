#ifndef REPR_H
#define REPR_H

#include "fixed_maths.h"
#include "maths.h"
#include "maths_tables.h"
#include "matrix.h"
#include <stdio.h>

char* repr_point2i(char* buffer, size_t count, Point2i* p);
char* repr_point2f(char* buffer, size_t count, Point2f* p);
char* repr_point3f(char* buffer, size_t count, Point3f* p);
char* repr_fixed_16_16(char* buffer, size_t count, int32_t f);
char* repr_mat_4x4(char* buffer, size_t count, int32_t s1[4][4]);
char* repr_mat_4x1(char* buffer, size_t count, int32_t s[4]);

#endif
