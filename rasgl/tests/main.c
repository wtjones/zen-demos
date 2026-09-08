#include "log.c/src/log.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/event.h"
#include "rasgl/core/fixed_maths.h"
#include "rasgl/core/model.h"
#include "rasgl/core/rasterize.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/string.h"
#include "rasgl/core/timer.h"
#include "test_support.h"
#include "tests.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * @brief Generic implementation for testing.
 * @return uint32_t Tick count
 */
uint32_t ras_timer_get_ticks(void)
{
    static uint32_t ticks = 0;
    return ticks++;
}

TEST(TEST_REPR_FIXED)
{
    char buffer[100];
    Point3f p = {
        .x = float_to_fixed_16_16(1.0),
        .y = float_to_fixed_16_16(1.125),
        .z = float_to_fixed_16_16(50.777)
    };

    ras_log_info("Point: %s\n", repr_point3f(buffer, 100, &p));
    return false;
}

TEST(TEST_REPR_MATRIX)
{

    char buffer[1000];
    RasFixed matrix[4][4];
    mat_set_identity_4x4(matrix);
    ras_log_info("Matrix: %s\n", repr_mat_4x4(buffer, sizeof buffer, matrix));

    return false;
}

TEST(TEST_CONSOLE)
{
    return false;
}

TEST(TEST_RASTERIZE_TRI)
{

    RasVector4f p0 = {
        .x = float_to_fixed_16_16(15.0f),
        .y = float_to_fixed_16_16(15.0f),
        .z = 0,
        .w = 1
    };

    RasVector4f p1 = {
        .x = float_to_fixed_16_16(21.0f),
        .y = float_to_fixed_16_16(20.0f),
        .z = 0,
        .w = 1
    };

    RasVector4f p2 = {
        .x = float_to_fixed_16_16(4.0f),
        .y = float_to_fixed_16_16(24.0f),
        .z = 0,
        .w = 1
    };

    RasVector4f* pv[3] = { &p0, &p1, &p2 };

    RasHorizontalLine lines[RAS_HORIZONTAL_LINE_MAX];
    size_t num_lines = rasterize_tri(pv, lines, sizeof(lines) / sizeof(lines[0]));

    ras_log_trace("Rasterize result of %zu lines...", num_lines);
    for (size_t i = 0; i < num_lines; i++) {
        char buffer1[255];
        char buffer2[255];
        ras_log_trace("LineL: %s LineR: %s\n",
            repr_point2i(buffer1, 255, &lines[i].left),
            repr_point2i(buffer2, 255, &lines[i].right));
    }
    return false;
}

TEST(TEST_STRING)
{
    assert(is_whitespace(""));
    assert(is_whitespace("    "));
    assert(!is_whitespace("FooBar"));
    assert(!is_whitespace("  Foo Bar "));
    return false;
}

TEST(TEST_EVENT_SUMMARY)
{
    ras_log_warn_ex(RAS_EVENT_SC_OBJ_CHANGE, "Event summary tests... num: %zu", 55);
    ras_log_warn_ex(RAS_EVENT_SC_OBJ_MOVE, "Event summary tests... num: %zu", 57);
    ras_log_warn_ex(RAS_EVENT_TEST_FIXTURE, "Event summary tests... num: %zu", 58);
    ras_log_buffer_ex(RAS_EVENT_TEST_FIXTURE, "Test buffer wrap %zu.", 88);

    ras_log_summary_flush();

    return false;
}

#define X(name) { #name, name },

TestFn test_fns[] = {
#include "TestList.inc"
};

#undef X

size_t num_fns = sizeof(test_fns) / sizeof(test_fns[0]);

int main(int argc, const char** argv)
{

    if (argc == 1) {
        fprintf(stderr, "Test name required.\n");
        return 1;
    }

    FILE* log_file = fopen("/tmp/rasgl.log", "w");

    log_add_fp(log_file, RAS_LOG_LEVEL_FILE);
    log_set_level(LOG_INFO);
    log_set_quiet(false);
    ras_log_init();

    ras_log_info("rasgl tests...\n");
    ras_log_trace("%s\n", "DEBUG = 1");

    TestFn* test_fn = test_fn_lookup(test_fns, num_fns, argv[1]);
    if (test_fn == NULL) {
        fprintf(stderr, "Error: Test %s not found.\n", argv[1]);
        return 1;
    }
    return test_fn->fn();

    // FIXME: Convert to ctest
    backface_tests();
    backface_tests2();
    pipeline_scene_tests();
    pack_tests();

    return 0;
}
