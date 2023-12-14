#include "rasgl/core/model.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/string.h"
#include <stdio.h>
#include <stdlib.h>

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

void core_model_group_init(RasModelGroup* group)
{
    group->name[0] = '\0';
    group->num_verts = 0;
    group->num_normals = 0;
    group->num_faces = 0;
}

int core_parse_group_name(char* line, RasModelGroup* group)
{
    char buffer[255];
    char *token, *str, *tofree;
    group->name[0] = '\0';

    // strsep() is destructive, so make a copy
    tofree = str = strdup(line);

    token = strsep(&str, " "); // g
    token = strsep(&str, " "); // name
    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_info("parsed from token: %s", token);
    strcat(group->name, token);
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
    ras_log_info("parsing float from token: %s", token);
    int32_t x = float_to_fixed_16_16(atof(token));
    ras_log_info("vector x: %s\n", repr_fixed_16_16(buffer, sizeof buffer, x));

    do {
        token = strsep(&str, " "); // space
    } while (token[0] == '\0');

    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_info("parsing float from token: %s", token);
    int32_t y = float_to_fixed_16_16(atof(token));
    ras_log_info("vector y: %s\n", repr_fixed_16_16(buffer, sizeof buffer, y));

    do {
        token = strsep(&str, " "); // space
    } while (token[0] == '\0');

    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_info("parsing float from token: %s", token);
    int32_t z = float_to_fixed_16_16(atof(token));
    ras_log_info("vector y: %s\n", repr_fixed_16_16(buffer, sizeof buffer, y));
    core_plat_free(&tofree);
    dest->x = x;
    dest->y = y;
    dest->z = z;
    ras_log_info("parsed vector: %s\n", repr_point3f(buffer, sizeof buffer, dest));
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

// void core_model_element_vert_init(RasModel* model)
// {
//     //    RasModelGroup* current_group;

//     for (int i = 0; i < model->num_groups; i++) {
//         RasModelGroup* current_group = &model->groups[i];
//         for (int j = 0; j < group->num_faces; j++) {
//             RasModelFace* face = &group->faces[j];
//             for (int k = 0; k < RAS_MAX_MODEL_FACE_INDEXES; k++) {
//                 RasModelFaceIndex* face_index = &face_index->indexes[k];
//                 RasVertex* element_vert = &group->element_verts[face_index->vert_index];
//                 element_vert->position->x = group->verts[face_index->vert_index].x;
//                 element_vert->position->x = group->verts[face_index->vert_index].y;
//                 element_vert->position->x = group->verts[face_index->vert_index].z;
//             }
//         }
//     }
// }

int core_load_model(char* path, RasModel* model)
{
    char buffer[255];
    FILE* file;
    char* line = NULL;
    size_t linesize = 0;

    file = fopen(path, "r");
    if (!file) {
        ras_log_error("Can't open file: %s\n", path);
        return -1;
    }

    strcat(model->name, path);
    // We default to the first group even if group isn't specifed. The actual
    // file group count is tracked to determine when to move past the default.
    int file_num_groups = 0;
    RasModelGroup* current_group = &model->groups[0];
    model->num_groups = 1;
    core_model_group_init(current_group);

    while (core_getline(&line, &linesize, file) != -1) {
        if (line[strlen(line) - 1] == '\n') {
            line[strlen(line) - 1] = '\0';
        }
        ras_log_trace("%lu : %s", strlen(line), line);

        if (strncmp(line, "g ", 2) == 0) {
            ras_log_info("group: %s", "");
            file_num_groups++;
            if (file_num_groups > 1) {
                ras_log_error("Multiple groups not supported.");
                return 1;
            }
            int result = core_parse_group_name(line, current_group);
            if (result != 0) {
                return result;
            }
        } else if (strncmp(line, "v ", 2) == 0) {
            ras_log_trace("vertex... %s", "");
            RasVector3f* v = &current_group->verts[current_group->num_verts];
            int result = core_parse_vector(line, v);
            if (result != 0) {
                return result;
            }
            current_group->num_verts++;
        } else if (strncmp(line, "vn ", 3) == 0) {
            ras_log_trace("vertex normal... %s", "");
            RasVector3f* v = &current_group->normals[current_group->num_normals];
            int result = core_parse_vector(line, v);
            if (result != 0) {
                return result;
            }
            current_group->num_normals++;
        } else if (strncmp(line, "f ", 2) == 0) {
            ras_log_trace("face... %s", "");
            RasModelFace* f = &current_group->faces[current_group->num_faces];
            int result = core_parse_face(line, f);
            if (result != 0) {
                return result;
            }
            current_group->num_faces++;
        }
        core_plat_free(&line);
    }
    core_plat_free(&line);

    core_model_element_vert_init(model);
    ras_log_info("%s\n", repr_model(buffer, sizeof buffer, model));
    return 0;
}
