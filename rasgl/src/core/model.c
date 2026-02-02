#include "rasgl/core/model.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/string.h"
#include <stdio.h>
#include <stdlib.h>

void core_model_init(RasModel* model)
{
    model->name = NULL;
    model->groups = NULL;
    model->num_groups = 0;
    model->materials = NULL;
    model->num_materials = 0;
}

void core_model_group_init(RasModelGroup* group)
{
    group->name = NULL;
    group->verts = NULL;
    group->num_verts = 0;
    group->normals = NULL;
    group->num_normals = 0;
    group->faces = NULL;
    group->num_faces = 0;
}

void core_free_model(RasModel* model)
{
    for (int i = 0; i < model->num_groups; i++) {
        RasModelGroup* group = &model->groups[i];
        free(group->name);
        free(group->verts);
        free(group->normals);
        free(group->faces);
    }
    free(model->groups);

    for (int i = 0; i < model->num_materials; i++) {
        RasModelMaterial* material = &model->materials[i];
        free(material->name);
    }
    free(model->materials);
    free(model->name);
    free(model);
}

RasResult core_model_group_to_pipeline_element_alloc(RasModelGroup* group, RasPipelineElement* element)
{
    return core_pipeline_element_alloc(
        group->num_verts,
        group->num_faces,
        group->num_faces * 3,
        group->num_faces,
        element);
}

void core_model_group_to_pipeline_element(RasModelGroup* group, RasPipelineElement* element)
{
    for (int i = 0; i < group->num_verts; i++) {
        RasVertex* element_vert = &element->verts[element->num_verts++];

        element_vert->position.x = group->verts[i].x;
        element_vert->position.y = group->verts[i].y;
        element_vert->position.z = group->verts[i].z;
    }

    core_get_element_aabb(element, &element->aabb);

    element->num_indexes = group->num_faces * 3;
    element->num_material_indexes = group->num_faces;
    element->num_faces = group->num_faces;
    uint32_t* element_index = &element->indexes[0];
    int32_t* material_index = &element->material_indexes[0];
    RasElementFace* dest_face = &element->faces[0];

    for (int j = 0; j < group->num_faces; j++) {
        RasModelFace* face = &group->faces[j];
        for (int k = 0; k < RAS_MAX_MODEL_FACE_INDEXES; k++) {
            RasModelFaceIndex* face_index = &face->indexes[k];
            *element_index = face_index->vert_index;
            element_index++;
        }

        *material_index = face->material_index;
        material_index++;

        dest_face->material_index = face->material_index;
        RasVector3f* src_normal = &group->normals[face->indexes[0].normal_index];
        dest_face->normal = *src_normal;
        dest_face++;
    }
}

char* core_repr_model(char* buffer, size_t count, RasModel* model)
{
    char buffer2[255];
    buffer[0] = '\0';
    snprintf(buffer2, sizeof buffer2, "model obj: %s bytes: %zu\n", model->name, sizeof(model));
    strcat(buffer, buffer2);

    for (int i = 0; i < model->num_groups; i++) {
        RasModelGroup* group = &model->groups[i];
        snprintf(buffer2, sizeof buffer2, "  group %d name: %s\n", i, group->name);
        strcat(buffer, buffer2);
        snprintf(buffer2, sizeof buffer2, "    num_verts: %d\n", group->num_verts);
        strcat(buffer, buffer2);
        snprintf(buffer2, sizeof buffer2, "    num_normals: %d\n", group->num_normals);
        strcat(buffer, buffer2);
        snprintf(buffer2, sizeof buffer2, "    num_faces: %d\n", group->num_faces);
        strcat(buffer, buffer2);
    }

    for (int i = 0; i < model->num_materials; i++) {
        RasModelMaterial* material = &model->materials[i];
        snprintf(buffer2, sizeof buffer2, "  material %d name: %s\n", i, material->name);
        strcat(buffer, buffer2);
    }
    return buffer;
}
