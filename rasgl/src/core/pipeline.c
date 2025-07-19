#include "rasgl/core/pipeline.h"
#include "rasgl/core/debug.h"

void* core_pipeline_run(RasPipeline* pipeline, void* input)
{
    void* data = input;
    for (int i = 0; i < pipeline->num_stages; i++) {
        void* data = input;
        RasPipelineStage* stage = &pipeline->stages[i];
        ras_log_debug("Stage: %s", stage->name);
        data = stage->fn(data);
    }
    return data;
}
