#ifndef MATRIX_PROJECTION_H
#define MATRIX_PROJECTION_H

#include "fixed_maths.h"
#include <stdint.h>

void mat_ortho_init(
    RasFixed matrix[4][4],
    RasFixed l,
    RasFixed r,
    RasFixed b,
    RasFixed t,
    RasFixed n,
    RasFixed f);

#endif
