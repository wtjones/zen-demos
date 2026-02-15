#include "rasgl/pack/pack.h"
#include <stdlib.h>

RasResult pack_decode_element(mpack_node_t node, RasPipelineElement* element)
{

    element->num_verts = mpack_node_u32(mpack_node_map_cstr(node, "num_verts"));
    element->verts = malloc(sizeof(RasVertex) * element->num_verts);
    if (element->verts == NULL) {
        ras_log_error("Failed to allocate memory for element verts");
        return RAS_RESULT_ERROR;
    }

    mpack_node_t verts_node = mpack_node_map_cstr(node, "verts");

    for (size_t i = 0; i < element->num_verts; i++) {
        pack_decode_vertex(mpack_node_array_at(verts_node, i), &element->verts[i]);
    }

    // TODO faces, indexes, material indexes

    mpack_node_t aabb_node = mpack_node_map_cstr(node, "aabb");
    if (pack_decode_aabb(aabb_node, &element->aabb) != RAS_RESULT_OK) {
        ras_log_error("Failed to decode element AABB");
        free(element->verts);
        return RAS_RESULT_ERROR;
    }

    return RAS_RESULT_OK;
}
