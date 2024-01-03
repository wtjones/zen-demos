#ifndef MODEL_H
#define MODEL_H

#include "debug.h"
#include "maths.h"

#define RAS_MAX_NAME 50
#define RAS_MAX_MODEL_GROUPS 1
#define RAS_MAX_MODEL_VERTS 1024
#define RAS_MAX_MODEL_FACES 256
#define RAS_MAX_MODEL_FACE_INDEXES 3
#define RAS_MAX_MODEL_NORMALS 256

typedef struct RasModelFaceIndex {
    int32_t vert_index;    // -1 if undefined
    int32_t texture_index; // -1 if undefined
    int32_t normal_index;  // -1 if undefined
} RasModelFaceIndex;

/**
 * The three vertices of a triangle
 */
typedef struct RasModelFace {
    RasModelFaceIndex indexes[RAS_MAX_MODEL_FACE_INDEXES];
} RasModelFace;

typedef struct RasModelGroup {
    char name[RAS_MAX_NAME];
    RasVector3f verts[RAS_MAX_MODEL_VERTS];
    uint32_t num_verts;
    RasVector3f normals[RAS_MAX_MODEL_NORMALS];
    uint32_t num_normals;
    RasModelFace faces[RAS_MAX_MODEL_FACES];
    uint32_t num_faces;
} RasModelGroup;

typedef struct RasModel {
    char name[RAS_MAX_NAME];
    RasModelGroup groups[RAS_MAX_MODEL_GROUPS];
    uint32_t num_groups;
} RasModel;

RasResult core_load_model(const char* path, RasModel* model);

char* core_repr_model(char* buffer, size_t count, RasModel* model);

#endif
