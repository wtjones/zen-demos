#ifndef PIPELINE_H
#define PIPELINE_H

#define RAS_MAX_PIPELINE_STAGES 24

typedef void* (*RasPipelineStageFn)(void* input);

typedef struct {
    const char* name;
    RasPipelineStageFn fn;
} RasPipelineStage;

typedef struct {
    RasPipelineStage stages[RAS_MAX_PIPELINE_STAGES];
    int num_stages;
} RasPipeline;

void* core_pipeline_run(RasPipeline* pipeline, void* input);

#endif
