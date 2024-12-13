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

void dump_maths_tables();
#endif
