#ifndef PACK_PACK_H
#define PACK_PACK_H

#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/model.h"
#include "rasgl/core/scene.h"

void pack_encode_vector3f(mpack_writer_t* writer, RasVector3f* vec);
RasResult pack_decode_vector3f(mpack_node_t node, RasVector3f* vec);

void pack_encode_vertex(mpack_writer_t* writer, RasVertex* vertex);
RasResult pack_decode_vertex(mpack_node_t node, RasVertex* vertex);

void pack_encode_face(mpack_writer_t* writer, RasPipelineFace* face);
RasResult pack_decode_face(mpack_node_t node, RasPipelineFace* face);

void pack_encode_aabb(mpack_writer_t* writer, RasAABB* aabb);
RasResult pack_decode_aabb(mpack_node_t node, RasAABB* aabb);

char* pack_encode_scene(RasScene* scene, size_t* out_size);
/**
 * @brief Decode a scene from mpack data.
 *
 * Localized free() is used for error handling as core_free_scene()
 * expects a fully initialized scene.
 *
 * @param data
 * @param size
 * @return RasScene*
 */
RasScene* pack_decode_scene(const char* data, size_t size);

void pack_encode_model(mpack_writer_t* writer, RasSceneModel* model);
RasResult pack_decode_scene_model(mpack_node_t node, RasSceneModel* model);

void pack_encode_camera(mpack_writer_t* writer, RasCamera* camera);
RasResult pack_decode_camera(mpack_node_t node, RasCamera* camera);

void pack_encode_object(mpack_writer_t* writer, RasSceneObject* obj);
RasResult pack_decode_scene_object(mpack_node_t node, RasSceneObject* obj);

void pack_encode_element(mpack_writer_t* writer, RasPipelineElement* element);
RasResult pack_decode_element(mpack_node_t node, RasPipelineElement* element);

#endif
