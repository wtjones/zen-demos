#include "rasgl/core/console.h"
#include "rasgl/core/repr.h"

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
    console->screen_pos.x = RAS_FIXED_ZERO;
    console->screen_pos.y = RAS_FIXED_ZERO;
    console->prompt_text[0] = '\0';

    return RAS_RESULT_OK;
}

void core_console_update(RasConsole* console, InputState* input_state)
{
    if (input_state->keys[RAS_KEY_RETURN] == RAS_KEY_EVENT_UP) {
        ras_log_info("Console enter key pressed.");
        char append[RAS_CONSOLE_DEFAULT_CAPACITY] = "";
        strcat(append, RAS_CONSOLE_PROMPT_CHAR);
        strcat(append, console->prompt_text);
        core_console_append(console, append);
        console->prompt_text[0] = '\0';
        core_console_append(console, "TODO: process command");
        return;
    }
    if (strlen(input_state->text) == 1 && input_state->text[0] != '`') {
        ras_log_info("Console text: %s", input_state->text);
        strcat(console->prompt_text, input_state->text);
        ras_log_info("Prompt text: \"%s\"", console->prompt_text);
        return;
    }
    if (input_state->keys[RAS_KEY_BACKSPACE] == RAS_KEY_EVENT_UP) {
        if (strlen(console->prompt_text) > 0) {
            console->prompt_text[strlen(console->prompt_text) - 1] = '\0';
        }
    }
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

RasResult console_trim(RasConsole* console, size_t append_count)
{
    ras_log_info("Console trim free count before: %d",
        console->buffer.max_count - core_console_count(console));
    if (append_count > console->buffer.max_count) {
        ras_log_error("Trim request exceeds max_count.");
        return RAS_RESULT_ERROR;
    }

    size_t* head = &console->buffer.head;

    // Allow for newline delimitter.
    bool done = console->buffer.max_count - core_console_count(console) >= append_count + 1;
    while (!done) {
        while (console->buffer.text[*head] != '\n') {
            (*head)++;
        }
        (*head)++; // Eat the newline;

        done = console->buffer.max_count - core_console_count(console) >= append_count + 1;
    }

    ras_log_info("Console trim free count after: %d",
        console->buffer.max_count - core_console_count(console));

    return RAS_RESULT_OK;
}

RasResult core_console_append(RasConsole* console, const char* str)
{
    const char* src = str;

    if (RAS_RESULT_ERROR == console_trim(console, strlen(str))) {
        ras_log_error("Unable to trim buffer head.");
        return RAS_RESULT_ERROR;
    }

    while (*src != '\0') {
        console->buffer.text[console->buffer.tail] = *src;
        console->buffer.tail = (console->buffer.tail + 1) % RAS_CONSOLE_DEFAULT_CAPACITY;
        src++;
    }
    console->buffer.text[console->buffer.tail] = '\n';
    console->buffer.tail = (console->buffer.tail + 1) % RAS_CONSOLE_DEFAULT_CAPACITY;

    return RAS_RESULT_OK;
}

RasResult core_console_build_index(RasConsole* console, RasConsoleLineIndex* line_index)
{
    line_index->count = 0;

    if (line_index == NULL) {
        ras_log_error("Null index");
        return RAS_RESULT_ERROR;
    }
    if (core_console_count(console) == 0) {
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

RasResult draw_console_text(RenderState* state, RasFont* font, RasConsole* console)
{
    Point2f pos = { .x = RAS_TEXT_LETTER_SPACING, .y = console->screen_pos.y };

    RasConsoleLineIndex index;
    RAS_CHECK_RESULT(core_console_build_index(console, &index));
    char buffer[RAS_CONSOLE_DEFAULT_CAPACITY];

    int32_t max_rows = console->visible_rows - 1; // Reserve prompt row
    int32_t start_row = (max_rows - (int32_t)index.count) < 0
        ? 0
        : max_rows - (int32_t)index.count;
    int32_t start_line = (int32_t)index.count - (max_rows - start_row);

    int32_t cur_line = start_line;
    int32_t cur_row = start_row;
    pos.y += start_row * (core_get_font_height(font) + RAS_TEXT_LETTER_SPACING);
    char b2[512];
    ras_log_buffer_info("max_rows: %d, cur_line: %d, cur_row: %d start pos: %s",
        max_rows, cur_line, cur_row, repr_point2f(b2, sizeof(b2), &pos));

    while (cur_row < max_rows) {
        repr_console_buffer_line(
            buffer,
            sizeof(buffer),
            console,
            index.line_starts[cur_line]);
        core_draw_text(state, font, pos, buffer);
        pos.y += core_get_font_height(font) + RAS_TEXT_LETTER_SPACING;
        cur_line++;
        cur_row++;
    }

    ras_log_buffer_info("Prompt pos: %s", repr_point2f(b2, sizeof(b2), &pos));
    ras_log_buffer_info("Prompt pos: %s", repr_fixed_16_16(b2, sizeof(b2), pos.y));

    core_draw_text(state, font, pos, RAS_CONSOLE_PROMPT_CHAR);
    pos.x += core_get_font_width(font, RAS_CONSOLE_PROMPT_CHAR);
    pos.x += RAS_TEXT_LETTER_SPACING;

    core_draw_text(state, font, pos, console->prompt_text);

    return RAS_RESULT_OK;
}

RasResult draw_console_bg(RenderState* state, RasFont* font, RasConsole* console)
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

RasResult core_draw_console(RenderState* state, RasFont* font, RasConsole* console)
{

    RAS_CHECK_RESULT(draw_console_bg(state, font, console));
    RAS_CHECK_RESULT(draw_console_text(state, font, console));

    return RAS_RESULT_OK;
}

char* repr_console_buffer(char* buffer, size_t count, RasConsole* console)
{
    size_t* tail = &console->buffer.tail;
    size_t current = console->buffer.head;
    char* dest = buffer;

    if (count < console->buffer.capacity) {
        ras_log_error("Given buffer should match size of console.");
        return NULL;
    }

    while (current != *tail) {
        *dest = console->buffer.text[current];
        current = (current + 1) % RAS_CONSOLE_DEFAULT_CAPACITY;
        dest++;
    }

    *dest = '\0';
    return buffer;
}

char* repr_console_buffer_line(char* buffer, size_t count, RasConsole* console, size_t start)
{
    size_t* tail = &console->buffer.tail;
    size_t current = start;
    char* dest = buffer;
    char* last = NULL;
    *dest = '\0';

    if (count < console->buffer.capacity) {
        ras_log_error("Given buffer should match size of console.");
        return NULL;
    }

    while (true) {
        *dest = console->buffer.text[current] == '\n'
            ? '\0'
            : console->buffer.text[current];
        if (*dest == '\0') {
            return buffer;
        }
        current = (current + 1) % RAS_CONSOLE_DEFAULT_CAPACITY;
        if (current == *tail) {
            ras_log_error("Should not reach tail");
            return NULL;
        }

        last = dest;
        dest++;
    }

    return buffer;
}
