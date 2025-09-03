#ifndef REPR_H
#define REPR_H

#include "camera.h"
#include "fixed_maths.h"
#include "graphics.h"
#include "maths.h"
#include "maths_tables.h"
#include "matrix.h"
#include <stdio.h>

char* repr_point2i(char* buffer, size_t count, Point2i* p);
char* repr_point2f(char* buffer, size_t count, Point2f* p);
char* repr_point3f(char* buffer, size_t count, Point3f* p);
char* repr_vector4f(char* buffer, size_t count, RasVector4f* p);
char* repr_fixed_16_16(char* buffer, size_t count, RasFixed f);
char* repr_mat_4x4(char* buffer, size_t count, RasFixed s1[4][4]);
char* repr_mat_4x1(char* buffer, size_t count, RasFixed s[4]);
const char* repr_clipping_mode(char* buffer, size_t count, RasClippingMode mode);
const char* repr_normal_mode(char* buffer, size_t count, RasNormalMode mode);
const char* repr_grid_mode(char* buffer, size_t count, RasGridMode mode);
const char* repr_camera(char* buffer, size_t count, RasCamera* camera);

#endif
