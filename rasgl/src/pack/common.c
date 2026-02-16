#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/model.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>

void pack_encode_vector3f(mpack_writer_t* writer, RasVector3f* vec)
{
    mpack_start_array(writer, 3);
    mpack_write_i32(writer, vec->x);
    mpack_write_i32(writer, vec->y);
    mpack_write_i32(writer, vec->z);
    mpack_finish_array(writer);
}

RasResult pack_decode_vector3f(mpack_node_t node, RasVector3f* vec)
{
    if (mpack_node_array_length(node) != 3) {
        ras_log_error("Invalid vector data");
        return RAS_RESULT_ERROR;
    }
    vec->x = mpack_node_i32(mpack_node_array_at(node, 0));
    vec->y = mpack_node_i32(mpack_node_array_at(node, 1));
    vec->z = mpack_node_i32(mpack_node_array_at(node, 2));
    return RAS_RESULT_OK;
}

void pack_encode_vertex(mpack_writer_t* writer, RasVertex* vertex)
{
    mpack_start_map(writer, 5);

    mpack_write_cstr(writer, "position");
    pack_encode_vector3f(writer, &vertex->position);

    mpack_write_cstr(writer, "color");
    mpack_write_u8(writer, vertex->color);

    mpack_write_cstr(writer, "u");
    mpack_write_i32(writer, vertex->u);

    mpack_write_cstr(writer, "v");
    mpack_write_i32(writer, vertex->v);

    mpack_write_cstr(writer, "normal");
    pack_encode_vector3f(writer, &vertex->normal);

    mpack_finish_map(writer);
}

RasResult pack_decode_vertex(mpack_node_t node, RasVertex* vertex)
{
    mpack_node_t position_node = mpack_node_map_cstr(node, "position");

    if (pack_decode_vector3f(position_node, &vertex->position) != RAS_RESULT_OK) {
        ras_log_error("Invalid vertex position data");
        return RAS_RESULT_ERROR;
    }

    vertex->color = (uint8_t)mpack_node_u8(mpack_node_map_cstr(node, "color"));
    vertex->u = mpack_node_i32(mpack_node_map_cstr(node, "u"));
    vertex->v = mpack_node_i32(mpack_node_map_cstr(node, "v"));

    mpack_node_t normal_node = mpack_node_map_cstr(node, "normal");
    if (pack_decode_vector3f(normal_node, &vertex->normal) != RAS_RESULT_OK) {
        ras_log_error("Invalid vertex normal data");
        return RAS_RESULT_ERROR;
    }

    return RAS_RESULT_OK;
}

void pack_encode_aabb(mpack_writer_t* writer, RasAABB* aabb)
{
    mpack_start_map(writer, 2);

    mpack_write_cstr(writer, "min");
    pack_encode_vector3f(writer, &aabb->min);

    mpack_write_cstr(writer, "max");
    pack_encode_vector3f(writer, &aabb->max);

    mpack_finish_map(writer);
}

RasResult pack_decode_aabb(mpack_node_t node, RasAABB* aabb)
{
    mpack_node_t min_node = mpack_node_map_cstr(node, "min");
    if (pack_decode_vector3f(min_node, &aabb->min) != RAS_RESULT_OK) {
        ras_log_error("Invalid AABB min data");
        return RAS_RESULT_ERROR;
    }

    mpack_node_t max_node = mpack_node_map_cstr(node, "max");
    if (pack_decode_vector3f(max_node, &aabb->max) != RAS_RESULT_OK) {
        ras_log_error("Invalid AABB max data");
        return RAS_RESULT_ERROR;
    }

    return RAS_RESULT_OK;
}
