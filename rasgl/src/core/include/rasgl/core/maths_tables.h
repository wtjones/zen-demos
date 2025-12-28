#ifndef MATHS_TABLES_H
#define MATHS_TABLES_H

#define _USE_MATH_DEFINES
#include "fixed_maths.h"
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>

#ifndef M_PI
#    define M_PI 3.14159265358979323846
#endif

extern RasFixed cos_table[360];
extern RasFixed sin_table[360];

#define RAS_COS(angle) (assert((angle) >= 0 && (angle) < 360), cos_table[(angle)])
#define RAS_SIN(angle) (assert((angle) >= 0 && (angle) < 360), sin_table[(angle)])

#define RAS_Z_SCALE_STEPS 1024
#define RAS_Z_SCALE_FIXED_MIN RAS_FIXED_TENTH
#define RAS_Z_SCALE_FIXED_MAX RAS_FLOAT_TO_FIXED(20.0f)

extern RasFixed z_scale_table[RAS_Z_SCALE_STEPS + 1];

void core_init_z_scale_table();

void dump_maths_tables();
#endif
