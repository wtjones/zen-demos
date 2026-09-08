
#include "rasgl/core/console.h"
#include "rasgl/core/debug.h"
#include "rasgl/core/line_buffer.h"
#include "rasgl/core/repr.h"
#include "test_support.h"
#include <math.h>
#include <stdio.h>

TEST(TEST_LINE_BUFFER_INDEX)
{
    RasLineBuffer* line_buffer = core_line_buffer_init(RAS_CONSOLE_DEFAULT_CAPACITY);
    assert(line_buffer != NULL);

    RasLineBufferIndex line_index;

    core_line_buffer_append(line_buffer, "Hi");
    core_line_buffer_append(line_buffer, "Hello world.");
    core_line_buffer_append(line_buffer, "More");

    RasResult result = core_line_buffer_build_index(line_buffer, &line_index);

    assert(result == RAS_RESULT_OK);
    assert(line_index.count == 3);
    core_line_buffer_free(line_buffer);
    return false;
}

TEST(TEST_RING_BUFFER)
{
    RasLineBuffer* line_buffer = core_line_buffer_init(RAS_LINE_BUFFER_MAX_COUNT);
    assert(line_buffer != NULL);

    RasLineBufferIndex line_index;
    line_index.max_count = RAS_LINE_BUFFER_MAX_COUNT;

    // Fill the buffer to force a wrap
    for (int i = 0; i < 3; ++i) {
        char line[32];
        snprintf(line, sizeof(line), "Line %d", i);
        RasResult result = core_line_buffer_append(line_buffer, line);
        assert(result == RAS_RESULT_OK);
    }

    RasResult result = core_line_buffer_build_index(line_buffer, &line_index);
    assert(result == RAS_RESULT_OK);
    assert(line_index.count == 3);

    // Add enough lines to wrap the buffer
    for (int i = 3; i < 10; ++i) {
        char line[32];
        snprintf(line, sizeof(line), "Line %d", i);
        RasResult result = core_line_buffer_append(line_buffer, line);
        assert(result == RAS_RESULT_OK);
    }
    result = core_line_buffer_build_index(line_buffer, &line_index);
    assert(result == RAS_RESULT_OK);
    // The count should not exceed max_count
    assert(line_index.count <= line_buffer->max_count);

    core_line_buffer_free(line_buffer);
    return false;
}

TEST(TEST_LINE_BUFFER_TRIM)
{
    RasLineBuffer* line_buffer = core_line_buffer_init(RAS_LINE_BUFFER_MAX_COUNT);
    assert(line_buffer != NULL);

    const char* small_str = "Not super big but enough to seed data.";
    const char* big_str = "Super big string that should force a trim if everything is working ok and this is a run on sentence.";

    RasResult result;
    size_t available_count = line_buffer->max_count - core_line_buffer_count(line_buffer);
    ras_log_info("LineBuffer available slots: %zu", available_count);

    while (available_count > strlen(big_str) + 1) {
        result = core_line_buffer_append(line_buffer, small_str);
        assert(result == RAS_RESULT_OK);
        available_count = line_buffer->max_count - core_line_buffer_count(line_buffer);
        ras_log_info("LineBuffer available slots: %zu", available_count);
    }

    result = core_line_buffer_append(line_buffer, big_str);
    assert(result == RAS_RESULT_OK);
    available_count = line_buffer->max_count - core_line_buffer_count(line_buffer);
    assert(available_count < RAS_LINE_BUFFER_MAX_COUNT);
    ras_log_info("After trimmed append: LineBuffer available slots: %zu", available_count);

    result = core_line_buffer_append(line_buffer, big_str);
    assert(result == RAS_RESULT_OK);
    result = core_line_buffer_append(line_buffer, big_str);
    assert(result == RAS_RESULT_OK);
    result = core_line_buffer_append(line_buffer, "Hello test 1 2 3!!!!!!!");
    assert(result == RAS_RESULT_OK);
    result = core_line_buffer_append(line_buffer, big_str);
    assert(result == RAS_RESULT_OK);
    result = core_line_buffer_append(line_buffer, big_str);
    assert(result == RAS_RESULT_OK);
    result = core_line_buffer_append(line_buffer, big_str);
    assert(result == RAS_RESULT_OK);

    char buffer[RAS_LINE_BUFFER_MAX_COUNT];

    ras_log_info("Buffer:");
    ras_log_info("\n%s",
        core_repr_line_buffer(buffer, RAS_LINE_BUFFER_MAX_COUNT, line_buffer));

    available_count = line_buffer->max_count - core_line_buffer_count(line_buffer);
    ras_log_info("LineBuffer available slots: %zu", available_count);

    core_line_buffer_free(line_buffer);
    return false;
}

TEST(TEST_LINE_BUFFER_REPR)
{
    RasLineBuffer* line_buffer = core_line_buffer_init(RAS_CONSOLE_DEFAULT_CAPACITY);
    assert(line_buffer != NULL);

    const char* small_str = "Not super big but enough to seed data.";
    const char* big_str = "Super big string that should force a trim if everything is working ok and this is a run on sentence.";

    RasLineBufferIndex line_index;

    core_line_buffer_append(line_buffer, small_str);
    core_line_buffer_append(line_buffer, "The 2nd line.");
    core_line_buffer_append(line_buffer, big_str);

    assert(RAS_RESULT_OK == core_line_buffer_build_index(line_buffer, &line_index));

    char buffer[RAS_CONSOLE_DEFAULT_CAPACITY];

    ras_log_info("Buffer\n%s",
        core_repr_line_buffer(buffer, RAS_CONSOLE_DEFAULT_CAPACITY, line_buffer));

    for (size_t i = 0; i < line_index.count; i++) {
        core_repr_line_buffer_line(
            buffer,
            RAS_CONSOLE_DEFAULT_CAPACITY,
            line_buffer,
            line_index.line_starts[i]);
        ras_log_info("Buffer line %zu:\n%s", i, buffer);
    }

    core_line_buffer_free(line_buffer);
    return false;
}
