#include "rasgl/core/debug.h"
#include "rasgl/core/model.h"
#include "rasgl/core/repr.h"
#include "rasgl/hosted/string.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * https://stackoverflow.com/questions/2234557/c-using-getline-prints-pointer-being-freed-was-not-allocated-in-xcode
 *
 * ¯\_(ツ)_/¯
 */
void core_plat_free(char** buffer)
{
#ifndef __APPLE__
    free(*buffer);
#endif
    *buffer = NULL;
}

int core_parse_group_name(char* line, RasModelGroup* group)
{
    char *token, *str, *tofree;

    // strsep() is destructive, so make a copy
    tofree = str = strdup(line);

    token = strsep(&str, " "); // g
    token = strsep(&str, " "); // name
    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_trace("parsed from token: %s", token);
    group->name = (char*)malloc(strlen(token) + 1);
    strcpy(group->name, token);
    core_plat_free(&tofree);
    return 0;
}

/**
 * Parse a vector line with format:
 * v  0.0  1.0  0.0
 */
int core_parse_vector(char* line, RasVector3f* dest)
{
    char buffer[255];
    char *token, *str, *tofree;

    // strsep() is destructive, so make a copy
    tofree = str = strdup(line);

    token = strsep(&str, " "); // v or vn

    do {
        token = strsep(&str, " "); // space
    } while (token[0] == '\0');

    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_trace("parsing float from token: %s", token);
    RasFixed x = float_to_fixed_16_16(atof(token));
    ras_log_trace("vector x: %s\n", repr_fixed_16_16(buffer, sizeof buffer, x));

    do {
        token = strsep(&str, " "); // space
    } while (token[0] == '\0');

    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_trace("parsing float from token: %s", token);
    RasFixed y = float_to_fixed_16_16(atof(token));
    ras_log_trace("vector y: %s\n", repr_fixed_16_16(buffer, sizeof buffer, y));

    do {
        token = strsep(&str, " "); // space
    } while (token[0] == '\0');

    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_trace("parsing float from token: %s", token);
    RasFixed z = float_to_fixed_16_16(atof(token));
    ras_log_trace("vector y: %s\n", repr_fixed_16_16(buffer, sizeof buffer, y));
    core_plat_free(&tofree);
    dest->x = x;
    dest->y = y;
    dest->z = z;
    ras_log_trace("parsed vector: %s\n", repr_point3f(buffer, sizeof buffer, dest));
    return 0;
}

/**
 * @brief Parse a usemtl line in the face section
 *
 * @param line
 * @param material
 * @return int
 */
int core_parse_face_material(char* line, RasModelMaterial* material)
{
    char *token, *str, *tofree;

    // strsep() is destructive, so make a copy
    tofree = str = strdup(line);

    token = strsep(&str, " "); // g
    token = strsep(&str, " "); // name
    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_trace("parsed from token: %s", token);
    material->name = (char*)malloc(strlen(token) + 1);

    strcpy(material->name, token);
    core_plat_free(&tofree);
    return 0;
}

/**
 * Parse the indexes from the face block v/t/n ie 6/3/4.
 */
int core_parse_face_index(char* line, RasModelFaceIndex* dest)
{
    char buffer[255];
    char *token, *str, *tofree;

    // strsep() is destructive, so make a copy
    tofree = str = strdup(line);

    token = strsep(&str, "/");
    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_trace("face vert index raw: %s", token);
    if (token[0] == '\0') {
        dest->vert_index = -1;
    } else {
        dest->vert_index = atoi(token) - 1;
    }
    ras_log_trace("face vert index int: %d", dest->vert_index);
    token = strsep(&str, "/");
    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_trace("face texture index raw: %s", token);
    if (token[0] == '\0') {
        dest->texture_index = -1;
        ras_log_trace("face texture not found");
    } else {
        dest->texture_index = atoi(token) - 1;
    }
    ras_log_trace("face texture index int: %d", dest->texture_index);
    token = strsep(&str, "/");
    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_trace("face normal index raw: %s", token);
    if (token[0] == '\0') {
        dest->normal_index = -1;
    } else {
        dest->normal_index = atoi(token) - 1;
    }
    ras_log_trace("face normal index int: %d", dest->normal_index);
    core_plat_free(&tofree);
    return 0;
}

/**
 * Parse the next raw face block in the face line ie 6//2.
 */
int core_parse_face_token(char** str, RasModelFaceIndex* dest)
{
    char* token;

    do {
        token = strsep(str, " "); // space
    } while (token[0] == '\0');

    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", *str);
        return 1;
    }
    ras_log_trace("parsing face index from token: %s", token);

    int result = core_parse_face_index(token, dest);
    if (result != 0) {
        return result;
    }

    return 0;
}

/**
 * Parse a vector line with format:
 * f  1//2  7//2  5//2
 */
int core_parse_face(char* line, RasModelFace* dest)
{
    char buffer[255];
    char *token, *str, *tofree;
    RasModelFaceIndex* face_index;
    // strsep() is destructive, so make a copy
    tofree = str = strdup(line);

    token = strsep(&str, " "); // f

    for (int i = 0; i < 3; i++) {
        face_index = &dest->indexes[i];
        int result = core_parse_face_token(&str, face_index);
        if (result != 0) {
            return result;
        }
    }

    core_plat_free(&tofree);
    return 0;
}

RasModel* core_load_model(const char* path)
{
    char buffer[255];
    FILE* file;
    char* line = NULL;
    size_t linesize = 0;

    file = fopen(path, "r");
    if (!file) {
        ras_log_error("Can't open file: %s\n", path);
        return NULL;
    }
    RasModel* model = malloc(sizeof(RasModel));
    if (model == NULL) {
        ras_log_error("Failed to allocate memory for model.");
        return NULL;
    }

    core_model_init(model);

    model->name = malloc(strlen(path) + 1);
    if (model->name == NULL) {
        ras_log_error("Failed to allocate memory for model name.");
        return NULL;
    }

    strcpy(model->name, path);
    // We default to the first group even if group isn't specifed. The actual
    // file group count is tracked to determine when to move past the default.

    int file_num_groups = 0;
    model->groups = malloc(sizeof(RasModelGroup));
    if (model->groups == NULL) {
        ras_log_error("Failed to allocate memory for model groups.");
        return NULL;
    }
    RasModelGroup* current_group = &model->groups[0];
    model->num_groups = 1;

    int32_t current_material_index = -1;

    core_model_group_init(current_group);

    while (core_getline(&line, &linesize, file) != -1) {
        if (line[strlen(line) - 1] == '\n') {
            line[strlen(line) - 1] = '\0';
        }
        ras_log_trace("%lu : %s", strlen(line), line);

        if (strncmp(line, "g ", 2) == 0) {
            ras_log_trace("group: %s", "");
            file_num_groups++;
            if (file_num_groups > 1) {
                ras_log_error("Multiple groups not supported.");
                return NULL;
            }
            int result = core_parse_group_name(line, current_group);
            if (result != 0) {
                return NULL;
            }
        } else if (strncmp(line, "v ", 2) == 0) {
            ras_log_trace("vertex... %s", "");
            current_group->verts = realloc(current_group->verts, sizeof(RasVector3f) * (current_group->num_verts + 1));
            RasVector3f* v = &current_group->verts[current_group->num_verts];
            int result = core_parse_vector(line, v);
            if (result != 0) {
                return NULL;
            }
            current_group->num_verts++;
        }

        else if (strncmp(line, "vn ", 3) == 0) {
            ras_log_trace("vertex normal... %s", "");
            current_group->normals = realloc(current_group->normals, sizeof(RasVector3f) * (current_group->num_normals + 1));
            RasVector3f* v = &current_group->normals[current_group->num_normals];
            int result = core_parse_vector(line, v);
            if (result != 0) {
                return NULL;
            }
            current_group->num_normals++;
        } else if (strncmp(line, "usemtl ", 7) == 0) {
            ras_log_trace("face material... %s", "");
            current_material_index = model->num_materials;
            model->num_materials++;
            model->materials = realloc(model->materials, sizeof(RasModelMaterial) * model->num_materials);
            RasModelMaterial* material = &model->materials[current_material_index];
            int result = core_parse_face_material(line, material);

        } else if (strncmp(line, "f ", 2) == 0) {

            // todo: ensure material is set

            ras_log_trace("face... %s", "");
            uint32_t current_face_index = current_group->num_faces;
            current_group->num_faces++;
            current_group->faces = realloc(current_group->faces, sizeof(RasModelFace) * (current_group->num_faces));
            RasModelFace* f = &current_group->faces[current_face_index];
            if (current_material_index == -1) {
                ras_log_warn("Material not set for face.");
            } else {
                ras_log_trace("Face material index: %d", current_material_index);
            }
            f->material_index = current_material_index;
            int result = core_parse_face(line, f);
            if (result != 0) {
                return NULL;
            }
        }
        core_plat_free(&line);
    }
    core_plat_free(&line);
    ras_log_trace("Model loaded: %s, bytes: %zu", model->name, sizeof *model);
    ras_log_info("%s", core_repr_model(buffer, sizeof buffer, model));
    return model;
    ;
}
