#ifndef MODEL_H
#define MODEL_H

#include "debug.h"
#include "maths.h"

#define RAS_MAX_MODEL_NAME 50
#define RAS_MAX_MODEL_GROUPS 1
#define RAS_MAX_MODEL_VERTS 1024
#define RAS_MAX_MODEL_FACES 256
#define RAS_MAX_MODEL_FACE_INDEXES 3
#define RAS_MAX_MODEL_NORMALS 256

typedef struct RasModelMaterial {
    char* name;
} RasModelMaterial;

typedef struct RasModelFaceIndex {
    RasFixed vert_index;    // -1 if undefined
    RasFixed texture_index; // -1 if undefined
    RasFixed normal_index;  // -1 if undefined
} RasModelFaceIndex;

/**
 * The three vertices of a triangle
 */
typedef struct RasModelFace {
    int32_t material_index;
    RasModelFaceIndex indexes[RAS_MAX_MODEL_FACE_INDEXES];
} RasModelFace;

typedef struct RasModelGroup {
    char* name;
    RasVector3f* verts;
    uint32_t num_verts;
    RasVector3f* normals;
    uint32_t num_normals;
    RasModelFace* faces;
    uint32_t num_faces;
} RasModelGroup;

typedef struct RasModel {
    char* name;
    RasModelGroup* groups;
    uint32_t num_groups;
    RasModelMaterial* materials;
    uint32_t num_materials;
} RasModel;

RasModel* core_load_model(const char* path);
void core_free_model(RasModel* model);
char* core_repr_model(char* buffer, size_t count, RasModel* model);

#endif
