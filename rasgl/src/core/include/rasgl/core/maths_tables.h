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

#define RAS_Z_SCALE_STEPS 2048
#define RAS_Z_SCALE_FIXED_MIN RAS_FIXED_TENTH
#define RAS_Z_SCALE_FIXED_MAX INT_32_TO_FIXED_16_16(32)
#define RAS_Z_SCALE_W_SHIFT 10 // W granularity of 0.015625
#define RAS_FOV_SCALE_STEPS 90 + 1
#define RAS_FOV_SCALE_OFFSET 30 // FOV from 30 to 120 degrees

extern RasFixed z_scale_table[RAS_Z_SCALE_STEPS + 1];
extern RasFixed z_scale_table_shift[RAS_Z_SCALE_STEPS + 1];
extern RasFixed fov_scale_table[RAS_FOV_SCALE_STEPS];

/**
 * @brief Initialize Z scale table.
 *
 */
void core_init_z_scale_table();

/**
 * @brief Initialize Z scale table using indexes shifted w value.
 *
 */
void core_init_z_scale_table_shift();

void core_init_maths_tables();

void dump_maths_tables();
#endif
