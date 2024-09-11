#ifndef UX_ARGS_H
#define UX_ARGS_H

#include "rexile/core/deck.h"
#include <stdbool.h>
#include <stdio.h>

typedef struct {
    bool verbose;
    char* initial_deck_file;

} UXOptions;

bool ux_parse_options(int argc, char* argv[], UXOptions* options);

#endif
