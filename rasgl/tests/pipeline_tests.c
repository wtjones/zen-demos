#include "rasgl/core/pipeline.h"
#include "tests.h"

typedef struct TestData {
    int value;
} TestData;

void* foo(void* input)
{
    TestData* data = (TestData*)input;
    ras_log_info("foo: %d", data->value);
    data->value *= 5;
    return input;
}

void* bar(void* input)
{
    TestData* data = (TestData*)input;
    ras_log_info("bar: %d", data->value);
    return input;
}

void pipeline_tests()
{

    RasPipeline pipeline = {
        .num_stages = 2,
        .stages = {
            { .name = "FOO!!", foo },
            { .name = "BAR!!!", bar } }
    };
    TestData data = { .value = 67 };
    core_pipeline_run(&pipeline, &data);
}
