#include "mpack/src/mpack.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/model.h"
#include "rasgl/pack/pack.h"
#include <stdlib.h>

void pack_encode_model(mpack_writer_t* writer, RasSceneModel* model)
{
    mpack_start_map(writer, 3);

    mpack_write_cstr(writer, "name");
    mpack_write_cstr(writer, model->name);
    mpack_write_cstr(writer, "path");
    mpack_write_cstr(writer, model->path);
    mpack_write_cstr(writer, "element");
    pack_encode_element(writer, &model->element);

    mpack_finish_map(writer);
}
