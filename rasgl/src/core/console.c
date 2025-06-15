#include "rasgl/core/console.h"

RasResult buffer_init(RasConsoleBuffer* buffer)
{
    buffer->text[0] = '\0';
    buffer->head = 0;
    buffer->tail = 0;
    buffer->capacity = RAS_CONSOLE_DEFAULT_CAPACITY;
    buffer->max_count = buffer->capacity - 1;
    return RAS_RESULT_OK;
}

RasResult core_console_init(RasConsole* console, ScreenSettings* settings)
{
    RAS_CHECK_RESULT(buffer_init(&console->buffer));
    console->visible_cols = RAS_CONSOLE_DEFAULT_COLS;
    console->visible_rows = RAS_CONSOLE_DEFAULT_ROWS;

    return RAS_RESULT_OK;
}

bool core_console_is_full(RasConsole* console)
{
    return console->buffer.head == (console->buffer.tail + 1) % console->buffer.capacity;
}

size_t core_console_count(RasConsole* console)
{
    // If tail has wrapped, add capacity to tail to calculate.
    //  T   H   t
    // 01234567xx
    return console->buffer.head <= console->buffer.tail
        ? console->buffer.tail - console->buffer.head
        : console->buffer.tail + console->buffer.capacity - console->buffer.head;
}

RasResult core_console_append(RasConsole* console, const char* str)
{
    const char* src = str;

    if (core_console_is_full(console)) {
        ras_log_error("Console buffer is full.");
        return RAS_RESULT_ERROR;
    }
    if (core_console_count(console) + strlen(str) > console->buffer.max_count) {
        ras_log_error("Line length of %d is would exeed buffer with count of %d",
            strlen(str),
            core_console_count(console));
    }

    while (*src != '\0') {
        console->buffer.text[console->buffer.tail++] = *src;
        src++;
    }
    console->buffer.text[console->buffer.tail++] = '\n';
    return RAS_RESULT_OK;
}

int32_t core_console_iter_char_rev(
    RasConsole* console, int32_t iterator, char* dest)
{
    if (iterator < 0) {
        ras_log_error("Invalid iterator: %d", iterator);
        return -1;
    }
    if (core_console_count(console) == 0) {
        ras_log_warn("Console empty.");
        *dest = '\0';
        return -1;
    }

    // FIXME: tail points to next unused

    *dest = console->buffer.text[iterator];

    // We were at the head, so indicate no more data;
    if (iterator == console->buffer.head) {
        return -1;
    }

    // Wrap ring buffer
    return iterator == 0
        ? console->buffer.capacity
        : iterator - 1;
}

int32_t core_console_iter_line_rev(
    RasConsole* console,
    int32_t iterator, char* dest, size_t count)
{
    int32_t result = -1;
    if (iterator < 0) {
        ras_log_error("Invalid iterator: %d", iterator);
        return -1;
    }
    if (console->buffer.text[iterator] != '\n') {
        ras_log_error("Line iterator must begin at a newline.");
        return -1;
    }

    dest[0] = '\0';
    if (core_console_count(console) == 0) {
        ras_log_warn("Console buffer empty.");
        return -1;
    }

    char ch;
    int32_t current = iterator - 1; // Start before the newline.
    current = core_console_iter_char_rev(
        console, current, &ch);

    while (current != -1 && ch != '\n') {
        size_t dest_len = strlen(dest);
        dest[dest_len] = ch;
        dest[dest_len + 1] = '\0';
        current = core_console_iter_char_rev(
            console, current, &ch);
    }

    return current;
}

RasResult core_console_build_index(RasConsole* console, RasConsoleLineIndex* line_index)
{
    line_index->count = 0;

    if (line_index == NULL) {
        ras_log_error("Null index");
        return RAS_RESULT_ERROR;
    }
    if (core_console_count(console) == 0) {
        ras_log_warn("Console empty.");
        return RAS_RESULT_OK;
    }

    int32_t current = console->buffer.head;
    int32_t last_start = console->buffer.head;
    size_t* ct = &line_index->count;

    while (true) {

        if (console->buffer.text[current] == '\n') {
            line_index->line_starts[(*ct)++] = last_start;

            current = (current + 1) % console->buffer.capacity;
            if (current == console->buffer.tail) {
                return RAS_RESULT_OK;
            }
            last_start = current;
            continue;
        }

        current = (current + 1) % console->buffer.capacity;
        if (current == console->buffer.tail) {
            ras_log_error("Unexpected that console buffer ends with non-newline.");
            return RAS_RESULT_ERROR;
        }
    }

    return RAS_RESULT_OK;
}

RasResult core_draw_console(RenderState* state, RasFont* font)
{
    RasPipelineVertex *pv0, *pv1, *pv2, *pv3;
    uint32_t pv0_i, pv1_i, pv2_i, pv3_i;
    uint32_t* si;
    uint32_t material_index = 0;

    // Top left pos of current char
    int32_t cur_x = 0;
    int32_t cur_y = 0;

    // Construct the console background with two triangles
    //  p0-------p2
    //  |  t0  / |
    //  |    /   |
    //  |  /  t1 |
    //  p1-------p3

    pv0 = &state->pipeline_verts[state->num_pipeline_verts];
    pv0_i = state->num_pipeline_verts;
    state->num_pipeline_verts++;

    pv1 = &state->pipeline_verts[state->num_pipeline_verts];
    pv1_i = state->num_pipeline_verts;
    state->num_pipeline_verts++;

    pv2 = &state->pipeline_verts[state->num_pipeline_verts];
    pv2_i = state->num_pipeline_verts;
    state->num_pipeline_verts++;

    pv3 = &state->pipeline_verts[state->num_pipeline_verts];
    pv3_i = state->num_pipeline_verts;
    state->num_pipeline_verts++;

    Point2f top_left = { .x = RAS_FIXED_ZERO, .y = RAS_FIXED_ZERO };
    Point2f bottom_right = {
        .x = INT_32_TO_FIXED_16_16(state->screen_settings.screen_width) - RAS_FIXED_ONE,
        .y = INT_32_TO_FIXED_16_16(state->screen_settings.screen_height) - RAS_FIXED_ONE
    };

    pv0->screen_space_position.x = top_left.x;
    pv0->screen_space_position.y = top_left.y;
    pv1->screen_space_position.x = top_left.x;
    pv1->screen_space_position.y = bottom_right.y;
    pv2->screen_space_position.x = bottom_right.x;
    pv2->screen_space_position.y = top_left.y;
    pv3->screen_space_position.x = bottom_right.x;
    pv3->screen_space_position.y = bottom_right.y;

    // uv0 (0,0) ------ uv1 (1,0)
    //    |               |
    //    |               |
    // uv2 (0,1) ------ uv3 (1,1)

    pv0->u = 0;
    pv0->v = 0;
    pv1->u = 1;
    pv1->v = 0;
    pv2->u = 0;
    pv2->v = 1;
    pv3->u = 1;
    pv3->v = 1;

    RasPipelineVertex* pvs[4] = { pv0, pv1, pv2, pv3 };

    state->material_indexes[state->num_material_indexes] = 0x2592;
    state->material_indexes[state->num_material_indexes + 1] = 0x2592;
    state->num_material_indexes += 2;

    si = &state->num_visible_indexes;

    state->visible_indexes[(*si)++] = pv0_i;
    state->visible_indexes[(*si)++] = pv1_i;
    state->visible_indexes[(*si)++] = pv2_i;
    state->visible_indexes[(*si)++] = pv2_i;
    state->visible_indexes[(*si)++] = pv1_i;
    state->visible_indexes[(*si)++] = pv3_i;

    return RAS_RESULT_OK;
}
