#include "rasgl/pack/pack.h"
#include <stdlib.h>

RasResult pack_decode_element(mpack_node_t node, RasPipelineElement* element)
{
    // Use 'commit-on-success' pattern to avoid having to free partially
    // decoded data on failure.
    // See also libglt2d's gltf parsing for a similar approach.
    RasPipelineElement tmp = { 0 };

    /* verts */
    tmp.num_verts = mpack_node_u32(mpack_node_map_cstr(node, "num_verts"));
    tmp.verts = malloc(sizeof(RasVertex) * tmp.num_verts);
    if (tmp.verts == NULL) {
        ras_log_error("Failed to allocate memory for element verts");
        return RAS_RESULT_ERROR;
    }

    mpack_node_t verts_node = mpack_node_map_cstr(node, "verts");
    for (size_t i = 0; i < tmp.num_verts; i++) {
        if (pack_decode_vertex(mpack_node_array_at(verts_node, i), &tmp.verts[i]) != RAS_RESULT_OK) {
            ras_log_error("Failed to decode vertex");
            goto fail;
        }
    }

    /* faces */
    tmp.num_faces = mpack_node_u32(mpack_node_map_cstr(node, "num_faces"));
    tmp.faces = NULL;
    if (tmp.num_faces > 0) {
        tmp.faces = malloc(sizeof(RasPipelineFace) * tmp.num_faces);
        if (tmp.faces == NULL) {
            ras_log_error("Failed to allocate memory for element faces");
            goto fail;
        }

        mpack_node_t faces_node = mpack_node_map_cstr(node, "faces");
        for (size_t i = 0; i < tmp.num_faces; i++) {
            mpack_node_t face_node = mpack_node_array_at(faces_node, i);
            if (pack_decode_face(face_node, &tmp.faces[i]) != RAS_RESULT_OK) {
                ras_log_error("Failed to decode face");
                goto fail;
            }
        }
    }

    /* indexes */
    tmp.num_indexes = mpack_node_u32(mpack_node_map_cstr(node, "num_indexes"));
    tmp.max_indexes = mpack_node_u32(mpack_node_map_cstr(node, "max_indexes"));
    tmp.indexes = NULL;
    if (tmp.num_indexes > 0) {
        tmp.indexes = malloc(sizeof(uint32_t) * tmp.num_indexes);
        if (tmp.indexes == NULL) {
            ras_log_error("Failed to allocate memory for element indexes");
            goto fail;
        }

        mpack_node_t indexes_node = mpack_node_map_cstr(node, "indexes");
        for (size_t i = 0; i < tmp.num_indexes; i++) {
            tmp.indexes[i] = (uint32_t)mpack_node_u32(mpack_node_array_at(indexes_node, i));
        }
    }

    /* material indexes */
    tmp.num_material_indexes = mpack_node_u32(mpack_node_map_cstr(node, "num_material_indexes"));
    tmp.material_indexes = NULL;
    if (tmp.num_material_indexes > 0) {
        tmp.material_indexes = malloc(sizeof(int32_t) * tmp.num_material_indexes);
        if (tmp.material_indexes == NULL) {
            ras_log_error("Failed to allocate memory for element material indexes");
            goto fail;
        }

        mpack_node_t matidx_node = mpack_node_map_cstr(node, "material_indexes");
        for (size_t i = 0; i < tmp.num_material_indexes; i++) {
            tmp.material_indexes[i] = mpack_node_i32(mpack_node_array_at(matidx_node, i));
        }
    }

    /* aabb */
    mpack_node_t aabb_node = mpack_node_map_cstr(node, "aabb");
    if (pack_decode_aabb(aabb_node, &tmp.aabb) != RAS_RESULT_OK) {
        ras_log_error("Failed to decode element AABB");
        goto fail;
    }

    /* commit */
    *element = tmp;
    return RAS_RESULT_OK;

fail:
    if (tmp.material_indexes)
        free(tmp.material_indexes);
    if (tmp.indexes)
        free(tmp.indexes);
    if (tmp.faces)
        free(tmp.faces);
    if (tmp.verts)
        free(tmp.verts);
    return RAS_RESULT_ERROR;
}
