#ifndef SCENE_H
#define SCENE_H

#include "debug.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop

#define MAX_SCENE_NAME 50

typedef struct {
    char name[MAX_SCENE_NAME];
} RasScene;

/**
 * @brief Load a scene from a larse script file
 *
 * @param path Path to the larse script file
 * @param scene Must be freed by caller
 * @return RasResult
 */
RasResult core_load_scene(const char* path, RasScene** scene);

#endif