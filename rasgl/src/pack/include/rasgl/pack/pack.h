#ifndef PACK_PACK_H
#define PACK_PACK_H

#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/model.h"
#include "rasgl/core/scene.h"

void pack_encode_vertex(mpack_writer_t* writer, RasVertex* vertex);
void pack_encode_face(mpack_writer_t* writer, RasPipelineFace* face);
void pack_encode_aabb(mpack_writer_t* writer, RasAABB* aabb);

char* pack_encode_scene(RasScene* scene, size_t* out_size);
RasScene* pack_decode_scene(const char* data, size_t size);

void pack_encode_model(mpack_writer_t* writer, RasSceneModel* model);
RasResult pack_decode_scene_model(mpack_node_t* node, RasSceneModel* model);

void pack_encode_element(mpack_writer_t* writer, RasPipelineElement* element);

#endif
