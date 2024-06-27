#ifndef SCENE_H
#define SCENE_H

#include "camera.h"
#include "debug.h"
#include "graphics.h"
#include "maths.h"
#include "model.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#include <larse/core/expression.h>
#include <larse/core/parse.h>
#include <larse/core/repr.h>
#pragma GCC diagnostic pop

#define MAX_SCENE_NAME 50
#define MAX_FILE_PATH 50
#define SCRIPT_SYMBOL_SCENE "scene"
#define SCRIPT_SYMBOL_PATH ":path"
#define SCRIPT_SYMBOL_MODEL "model"
#define SCRIPT_SYMBOL_MODEL_NAME ":model_name"
#define SCRIPT_SYMBOL_MODELS "models"
#define SCRIPT_SYMBOL_OBJECT "object"
#define SCRIPT_SYMBOL_OBJECTS "objects"
#define SCRIPT_SYMBOL_CAMERA "camera"
#define SCRIPT_SYMBOL_CAMERAS "cameras"
#define SCRIPT_SYMBOL_ANGLE ":angle"
#define SCRIPT_SYMBOL_POSITION ":position"
#define SCRIPT_SYMBOL_ORIENTATION ":orientation"
#define SCRIPT_SYMBOL_ROTATION_DELTA ":rotation_delta"
#define SCRIPT_SYMBOL_VEC "vec"
#define SCRIPT_SYMBOL_NAME ":name"

/**
 * @brief Represents a RasModel in a scene
 * Must be freed via core_free_scene_model()
 *
 */
typedef struct {
    char name[MAX_SCENE_NAME]; // Reference name of the model
    char path[MAX_FILE_PATH];
    RasPipelineElement element; // Scene-level elements converted from RasModel
} RasSceneModel;

typedef struct {
    /**
     * @brief The model referenced via :name in script is converted to
     * a pipeline element and referenced here.
     *
     */
    RasPipelineElement* element_ref;
    RasVector3f position;
    RasVector3f rotation;       // Intialized from :orientation in scene script
    RasVector3f rotation_delta; // Rotation speed
} RasSceneObject;

typedef struct {
    char name[MAX_SCENE_NAME];
    RasSceneModel* models;
    size_t num_models;
    RasSceneObject* objects;
    size_t num_objects;
    RasCamera* cameras;
    size_t num_cameras;

} RasScene;

/**
 * @brief Load a scene from a larse script file
 *
 * @param path Path to the larse script file
 * @param scene Must be freed by caller
 * @return RasResult
 */
RasResult core_load_scene(const char* path, RasScene** scene);

void core_free_scene(RasScene** scene);

RasResult core_script_map_vec(LarNode* node, RasVector3f* vec);

/**
 * @brief Map a scene script to a RasScene
 *
 * @param script The parsed larse script
 * @param scene Must be freed by caller
 * @return RasResult
 */
RasResult core_script_map_scene(LarScript* script, RasScene** scene);

#endif
