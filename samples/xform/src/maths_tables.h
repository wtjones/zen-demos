#ifndef MATHS_TABLES_H
#define MATHS_TABLES_H

#define _USE_MATH_DEFINES
#include <math.h>
#include <stdbool.h>
#include <stdint.h>

#ifndef M_PI
#    define M_PI 3.14159265358979323846
#endif

extern int32_t cos_table[360];
extern int32_t sin_table[360];

void dump_maths_tables();
#endif
