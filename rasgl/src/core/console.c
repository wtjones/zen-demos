#include "rasgl/core/console.h"
#include "rasgl/core/repr.h"
#include "rasgl/core/string.h"

RasResult history_init(RasConsole* console)
{
    console->history = core_line_buffer_init(RAS_CONSOLE_DEFAULT_CAPACITY);
    if (console->history == NULL) {
        return RAS_RESULT_ERROR;
    }
    console->history_recall_depth = RAS_CONSOLE_HISTORY_DEPTH_DEFAULT;
    return RAS_RESULT_OK;
}

RasConsole* core_console_init(ScreenSettings* settings)
{
    RasConsole* console = malloc(sizeof(RasConsole));
    if (console == NULL) {
        return NULL;
    }
    console->buffer = core_line_buffer_init(RAS_CONSOLE_DEFAULT_CAPACITY);
    if (console->buffer == NULL) {
        return NULL;
    }
    if (history_init(console) == RAS_RESULT_ERROR) {
        return NULL;
    }
    console->visible_cols = RAS_CONSOLE_DEFAULT_COLS;
    console->visible_rows = RAS_CONSOLE_DEFAULT_ROWS;
    console->screen_pos.x = RAS_FIXED_ZERO;
    console->screen_pos.y = RAS_FIXED_ZERO;
    console->prompt_text[0] = '\0';

    return console;
}

void core_console_free(RasConsole* console)
{
    core_line_buffer_free(console->buffer);
    core_line_buffer_free(console->history);
    free(console);
}

void on_history_recall(RasConsole* console, RasLineBufferIndex* index)
{

    ras_log_info("console->history_depth: %d", console->history_recall_depth);

    char buffer[RAS_CONSOLE_DEFAULT_CAPACITY];
    int32_t offset = (index->count - 1) + console->history_recall_depth;
    int32_t last_line = index->line_starts[offset];

    core_repr_line_buffer_line(
        buffer, sizeof(buffer), console->history, last_line);
    ras_log_info("History: %s", buffer);
    console->prompt_text[0] = '\0';
    strcat(console->prompt_text, buffer);
}

void on_history_recall_forward(RasConsole* console)
{
    console->history_recall_depth += console->history_recall_depth == RAS_CONSOLE_HISTORY_DEPTH_DEFAULT
        ? 0
        : 1;

    if (console->history_recall_depth == RAS_CONSOLE_HISTORY_DEPTH_DEFAULT) {
        ras_log_info("History recall at default");
        console->prompt_text[0] = '\0';
        return;
    }

    RasLineBufferIndex index;
    core_line_buffer_build_index(console->history, &index);

    on_history_recall(console, &index);
}

void on_history_recall_back(RasConsole* console)
{
    RasLineBufferIndex index;
    core_line_buffer_build_index(console->history, &index);
    if (index.count == 0) {
        ras_log_info("History empty.");
        return;
    }

    console->history_recall_depth -= abs(console->history_recall_depth - 1) == index.count
        ? 0
        : 1;

    on_history_recall(console, &index);
}

void on_history_update(RasConsole* console)
{
    if (is_whitespace(console->prompt_text)) {
        return;
    }

    RasLineBufferIndex index;
    core_line_buffer_build_index(console->history, &index);

    if (index.count == 0) {
        core_line_buffer_append(console->history, console->prompt_text);
        return;
    }

    // Don't add a dupe.
    char buffer[RAS_CONSOLE_DEFAULT_CAPACITY];
    int32_t last_line = index.line_starts[index.count - 1];

    core_repr_line_buffer_line(
        buffer, sizeof(buffer), console->history, last_line);

    if (strcmp(buffer, console->prompt_text) == 0) {
        ras_log_info("Command same as prior. Not adding to history.");
        return;
    }
    core_line_buffer_append(console->history, console->prompt_text);
    // Reset recall to first item.
    console->history_recall_depth = RAS_CONSOLE_HISTORY_DEPTH_DEFAULT;
}

void core_console_update(RasConsole* console, InputState* input_state)
{

    if (input_state->keys[RAS_KEY_UP] == RAS_KEY_EVENT_UP) {
        on_history_recall_back(console);
        return;
    }

    if (input_state->keys[RAS_KEY_DOWN] == RAS_KEY_EVENT_UP) {
        on_history_recall_forward(console);
        return;
    }

    if (input_state->keys[RAS_KEY_RETURN] == RAS_KEY_EVENT_UP) {
        ras_log_info("Console enter key pressed.");
        char append[RAS_CONSOLE_DEFAULT_CAPACITY] = "";
        strcat(append, RAS_CONSOLE_PROMPT_CHAR);
        strcat(append, console->prompt_text);
        core_line_buffer_append(console->buffer, append);
        on_history_update(console);
        console->prompt_text[0] = '\0';

        core_line_buffer_append(console->buffer, "TODO: process command");
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

RasResult draw_console_text(RenderState* state, RasFont* font, RasConsole* console)
{
    Point2f pos = { .x = RAS_TEXT_LETTER_SPACING, .y = console->screen_pos.y };

    RasLineBufferIndex index;
    RAS_CHECK_RESULT(core_line_buffer_build_index(console->buffer, &index));
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
        core_repr_line_buffer_line(
            buffer,
            sizeof(buffer),
            console->buffer,
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

    if (state->current_frame / RAS_CONSOLE_BLINK_RATE % 2 == 0) {
        pos.x += (core_get_font_width(font, console->prompt_text));
        pos.x += core_get_font_width(font, "");
        core_draw_text(state, font, pos, RAS_CONSOLE_PROMPT_CURSOR);
    }
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
