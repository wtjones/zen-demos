#ifndef REPR_H
#define REPR_H

#include "fixed_maths.h"
#include "maths.h"
#include "maths_tables.h"
#include "matrix.h"

char* repr_point3f(char* buffer, size_t count, Point3f* p);
char* repr_mat_4x4(char* buffer, size_t count, int32_t s1[4][4]);

#endif
