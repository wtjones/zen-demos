#include "rasgl/core/model.h"
#include "rasgl/core/debug.h"
#include <stdio.h>
#include <stdlib.h>

/**
 * https://stackoverflow.com/questions/2234557/c-using-getline-prints-pointer-being-freed-was-not-allocated-in-xcode
 *
 * ¯\_(ツ)_/¯
 */
void core_plat_free(char* buffer)
{
#ifndef __APPLE__
    free(buffer);
#endif
}
void core_model_group_init(RasModelGroup* group)
{
    group->name[0] = '\0';
    group->num_verts = 0;
    group->num_normals = 0;
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
    core_plat_free(tofree);
    return 0;
}

/**
 * Parse a vector line with format:
 * v  0.0  1.0  0.0
 */
int core_parse_vertex(char* line, RasVector3f* dest)
{
    char buffer[255];
    char *token, *str, *tofree;

    // strsep() is destructive, so make a copy
    tofree = str = strdup(line);

    token = strsep(&str, " "); // v
    token = strsep(&str, " "); // space
    token = strsep(&str, " "); // x
    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_info("parsing float from token: %s", token);
    int32_t x = float_to_fixed_16_16(atof(token));
    ras_log_info("vector x: %s\n", repr_fixed_16_16(buffer, sizeof buffer, x));

    token = strsep(&str, " "); // space
    token = strsep(&str, " "); // y
    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_info("parsing float from token: %s", token);
    int32_t y = float_to_fixed_16_16(atof(token));
    ras_log_info("vector y: %s\n", repr_fixed_16_16(buffer, sizeof buffer, y));

    token = strsep(&str, " "); // space
    token = strsep(&str, " "); // z
    if (token == NULL) {
        ras_log_error("strsep() returned NULL %s", str);
        return 1;
    }
    ras_log_info("parsing float from token: %s", token);
    int32_t z = float_to_fixed_16_16(atof(token));
    ras_log_info("vector y: %s\n", repr_fixed_16_16(buffer, sizeof buffer, y));
    core_plat_free(tofree);
    dest->x = x;
    dest->y = y;
    dest->z = z;
    ras_log_info("parsed vector: %s\n", repr_point3f(buffer, sizeof buffer, dest));
    return 0;
}

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

    while (getline(&line, &linesize, file) != -1) {
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
            ras_log_info("vector: %s", "");
            RasVector3f* v = &current_group->verts[current_group->num_verts];
            int result = core_parse_vertex(line, v);
            if (result != 0) {
                return result;
            }
            current_group->num_verts++;
        }
        core_plat_free(line);
    }
    core_plat_free(line);

    ras_log_info("%s\n", repr_model(buffer, sizeof buffer, model));
    return 0;
}
