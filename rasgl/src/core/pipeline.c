#include "rasgl/core/pipeline.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/timer.h"

void* core_pipeline_run(RasPipeline* pipeline, void* input)
{
    void* data = input;
    uint32_t max_duration = 0;
    for (int i = 0; i < pipeline->num_stages; i++) {
        RasPipelineStage* stage = &pipeline->stages[i];
        stage->last_start_ticks = ras_timer_get_ticks();
        data = stage->fn(input);
        stage->last_end_ticks = ras_timer_get_ticks();
        uint32_t duration = stage->last_end_ticks - stage->last_start_ticks;
        max_duration = duration > max_duration ? duration : max_duration;
    }

    // Report on slowest 33% of stages.
    uint32_t threshold = max_duration - (max_duration >> 1);
    for (int i = 0; i < pipeline->num_stages; i++) {
        RasPipelineStage* stage = &pipeline->stages[i];
        uint32_t duration = stage->last_end_ticks - stage->last_start_ticks;
        if (duration < threshold) {
            continue;
        }
        ras_log_buffer(
            "Stage: %s duration %u ticks",
            stage->name,
            duration);
    }
    return data;
}
